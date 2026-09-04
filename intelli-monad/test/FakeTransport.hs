{-# LANGUAGE OverloadedStrings #-}

-- | An in-memory 'Transport' pair for tests: whatever the client side
-- sends, the test's \"server side\" can take and inspect; whatever the
-- server side sends, the client receives. EOF semantics on close match
-- the transport contract in "IntelliMonad.MCP.Transport".
--
-- This is the contract every real transport (stdio now, Streamable
-- HTTP in Phase 2) must honor, exercised without spawning processes.

module FakeTransport
  ( FakeServer (..)
  , newFakePair
  , serverNextMessage
  , serverSendValue
  , serverSendRaw
  , serverReplyResult
  , serverReplyError
  ) where

import Control.Concurrent.STM
import qualified Data.Aeson as A
import Data.Aeson ((.=), object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

import IntelliMonad.MCP.Framing (encodeMessage)
import IntelliMonad.MCP.Transport (Transport (..))

-- | The test-side handle of a fake transport pair.
data FakeServer = FakeServer
  { fsFromClient :: TQueue BS.ByteString
    -- ^ Frames the client sent, in order. One 'serverNextMessage' take
    -- per frame.
  , fsToClient :: TQueue BS.ByteString
    -- ^ Chunks waiting for the client's 'tRecv' (one queued frame =
    -- one delivered chunk; real transports chunk arbitrarily, but the
    -- client's assembler makes that invisible here).
  , fsSealed :: TVar Bool
    -- ^ Set by close: client 'tRecv' then returns 'Nothing' forever.
  , fsClient :: Transport
  }

-- | Create a connected pair: a 'Transport' for the client session and
-- the 'FakeServer' handle the test drives.
newFakePair :: IO FakeServer
newFakePair = do
  fromClient <- newTQueueIO
  toClient <- newTQueueIO
  sealed <- newTVarIO False
  let send frame = atomically $ do
        s <- readTVar sealed
        unless s (writeTQueue fromClient frame)
      recv = atomically $ do
        s <- readTVar sealed
        if s
          then pure Nothing
          else Just <$> readTQueue toClient
      close = atomically (writeTVar sealed True)
  pure
    FakeServer
      { fsFromClient = fromClient
      , fsToClient = toClient
      , fsSealed = sealed
      , fsClient = Transport send recv close
      }
  where
    unless b a = if b then pure () else a

-- | Take the next frame the client sent (blocks; bound it with
-- 'System.Timeout.timeout' in tests that assert absence).
serverNextMessage :: FakeServer -> IO BS.ByteString
serverNextMessage fs = atomically (readTQueue (fsFromClient fs))

-- | Queue a raw chunk for the client to receive.
serverSendRaw :: FakeServer -> BS.ByteString -> IO ()
serverSendRaw fs = atomically . writeTQueue (fsToClient fs)

-- | Queue an encoded JSON value (a complete frame).
serverSendValue :: A.ToJSON v => FakeServer -> v -> IO ()
serverSendValue fs v = serverSendRaw fs (encodeMessage v)

-- | Queue a successful response for the given request id.
serverReplyResult :: FakeServer -> A.Value -> A.Value -> IO ()
serverReplyResult fs rid result =
  serverSendValue fs (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result])

-- | Queue a JSON-RPC error response for the given request id.
serverReplyError :: FakeServer -> A.Value -> Int -> Text -> IO ()
serverReplyError fs rid code msg =
  serverSendValue fs
    (object
       [ "jsonrpc" .= ("2.0" :: Text)
       , "id" .= rid
       , "error" .= object ["code" .= code, "message" .= msg]
       ])
