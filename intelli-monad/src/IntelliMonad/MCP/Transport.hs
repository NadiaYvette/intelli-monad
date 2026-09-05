{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Transports for MCP client sessions.
--
-- A transport is a record of three IO actions: send one encoded frame,
-- receive the next raw chunk, close. A record of functions (rather than
-- a type class) keeps the client's interface concrete — everything the
-- session loop in "IntelliMonad.MCP.Client" needs is these three
-- actions, and fake transports for tests are a few lines.
--
-- Contract:
--
--   * 'tSend' is safe from concurrent threads (implementations must
--     serialize).
--   * 'tRecv' returns 'Nothing' at end of stream. It is interruptible:
--     the client bounds every receive with a timeout slice so shutdown
--     and deadlines never wedge on a blocked read.
--   * 'tClose' is idempotent. After 'tClose', 'tRecv' returns 'Nothing'
--     and 'tSend' does not throw (it may be a no-op).

module IntelliMonad.MCP.Transport
  ( -- * Transport interface
    Transport (..)
    -- * stdio transport
  , StdIOConfig (..)
  , defaultStdIOConfig
  , openStdIO
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBuffering)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)

-- | A bidirectional message channel to an MCP peer.
data Transport = Transport
  { tSend :: BS.ByteString -> IO ()
    -- ^ Send one frame (bytes include the framing newline; the client
    -- uses 'IntelliMonad.MCP.Framing.encodeMessage').
  , tRecv :: IO (Maybe BS.ByteString)
    -- ^ Receive the next raw chunk (arbitrary boundaries). 'Nothing' at
    -- end of stream.
  , tClose :: IO ()
    -- ^ Close the channel; idempotent.
  }

--------------------------------------------------------------------------------
-- stdio
--------------------------------------------------------------------------------

-- | How to spawn an MCP server as a subprocess. This is how Claude
-- Desktop, Hermes, and @pty-mcp-server@ are all launched.
data StdIOConfig = StdIOConfig
  { sioProgram :: FilePath
    -- ^ Executable (resolved via @PATH@).
  , sioArgs :: [String]
  , sioEnv :: Maybe [(String, String)]
    -- ^ 'Nothing' inherits the parent environment.
  , sioCwd :: Maybe FilePath
  }

-- | A stdio config with no args, no env override, no cwd change.
defaultStdIOConfig :: FilePath -> StdIOConfig
defaultStdIOConfig prog = StdIOConfig prog [] Nothing Nothing

-- | Run an action, swallowing every exception. Used on the shutdown
-- path, where the first failure must not prevent the remaining
-- cleanup steps.
graceful :: IO () -> IO ()
graceful a = do
  r <- try a
  case r of
    Right () -> pure ()
    Left (_ :: SomeException) -> pure ()

-- | Spawn the server process and wrap its stdin/stdout as a 'Transport'.
--
-- Shutdown is graceful: close stdin (the conventional EOF signal for a
-- stdio server), give the process two seconds to exit on its own, then
-- terminate it if it has not. The process is always reaped and every
-- handle is always closed, even when individual steps fail.
openStdIO :: StdIOConfig -> IO Transport
openStdIO cfg = do
  sendLock <- newMVar ()
  closedOnce <- newMVar False
  let cp = (proc (sioProgram cfg) (sioArgs cfg))
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        , env = sioEnv cfg
        , cwd = sioCwd cfg
        }
  (Just hin, Just hout, _, ph) <- createProcess cp
  hSetBuffering hin (BlockBuffering Nothing)
  hSetBuffering hout (BlockBuffering Nothing)
  let send frame = withMVar sendLock $ \_ -> do
        -- Never throws (transport contract): after close the handle is
        -- shut and a write would fail; dropping the frame beats
        -- crashing a caller racing shutdown.
        r <- try (BS.hPut hin frame >> hFlush hin)
        case r of
          Right () -> pure ()
          Left (_ :: SomeException) -> pure ()
      recv = do
        r <- try (BS.hGetSome hout 65536)
        case r of
          Right bs
            | BS.null bs -> pure Nothing
            | otherwise -> pure (Just bs)
          Left (_ :: SomeException) -> pure Nothing
      close = modifyMVar_ closedOnce $ \done ->
        if done
          then pure done
          else do
            graceful (hClose hin)
            -- 2s grace for the server to exit on EOF before we kill it.
            _ <- timeout 2000000 (waitForProcess ph)
            graceful (terminateProcess ph)
            graceful (hClose hout)
            pure True
  pure (Transport send recv close)
