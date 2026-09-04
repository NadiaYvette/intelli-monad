{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The MCP client session.
--
-- Wires the pure Phase-1 pieces into a live protocol session:
--
--   * a reader thread pumps 'IntelliMonad.MCP.Transport.tRecv' through
--     the 'IntelliMonad.MCP.Framing' assembler, decodes each line, and
--     classifies it with 'IntelliMonad.MCP.Correlate';
--   * responses unblock the waiting 'mcpRequest' caller (out-of-order
--     safe — every request gets its own completion MVar, and timeout
--     vs. late response is decided atomically in the pending map);
--   * server-initiated requests (@sampling\/createMessage@,
--     @roots\/list@, @elicitation\/create@) are answered by a
--     user-installed handler, or refused with JSON-RPC
--     method-not-found;
--   * 'initialize' performs the 2025-06-18 handshake via
--     "IntelliMonad.MCP.Negotiate" and caches the outcome.
--
-- The reader thread never throws: undecodable input is logged to
-- stderr and skipped, keeping the stream alive.

module IntelliMonad.MCP.Client
  ( -- * Sessions
    Session
  , newSession
  , closeSession
    -- * Handshake
  , initialize
  , negotiated
    -- * Requests and notifications
  , mcpRequest
  , mcpNotify
  , defaultRequestTimeoutMs
    -- * Server-initiated requests
  , respondToServer
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Data.Aeson as A
import Data.Aeson ((.=), object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import IntelliMonad.MCP.Correlate
import IntelliMonad.MCP.Framing
import IntelliMonad.MCP.Negotiate
import IntelliMonad.MCP.Transport

--------------------------------------------------------------------------------
-- Session state
--------------------------------------------------------------------------------

data ClientState = Starting | Running | Closed
  deriving (Eq, Show)

-- | One open MCP session over one 'Transport'.
--
-- Use 'newSession' + 'initialize', then 'mcpRequest' / 'mcpNotify'.
data Session = Session
  { sTransport :: Transport
  , sPending :: MVar (IM.IntMap (A.Object -> IO ()))
    -- ^ Open request ids → the callback that completes their waiter.
    -- All registration/removal decisions (including the timeout-vs-
    -- late-response race) are made atomically under this lock.
  , sNextId :: IORef Int -- ^ Last allocated id; first request gets 1 (Correlate's convention: id 0 is legal but invites off-by-one confusion).
  , sServerHandler :: MVar (Maybe (Text -> A.Object -> IO A.Value))
  , sNegotiated :: MVar (Maybe Negotiation)
  , sState :: IORef ClientState
  , sReader :: IORef (Maybe SomeThreadId)
  }

-- | Opaque wrapper so the exported Session type does not leak
-- Control.Concurrent.
newtype SomeThreadId = SomeThreadId ThreadId

-- | Default bound for a request: 30 seconds. MCP servers are
-- interactive; anything slower deserves an explicit override.
defaultRequestTimeoutMs :: Int
defaultRequestTimeoutMs = 30000

-- | Wrap a transport in a session and start the reader thread.
newSession :: Transport -> IO Session
newSession trans = do
  pending <- newMVar IM.empty
  nextId <- newIORef 0
  handler <- newMVar Nothing
  neg <- newMVar Nothing
  st <- newIORef Starting
  rd <- newIORef Nothing
  let s = Session trans pending nextId handler neg st rd
  tid <- forkIO (readerLoop s)
  writeIORef rd (Just (SomeThreadId tid))
  pure s

-- | Stop the reader and close the transport. Idempotent.
closeSession :: Session -> IO ()
closeSession s = do
  old <- atomicModifyIORef' (sState s) (\st' -> (Closed, st'))
  when (old /= Closed) $ do
    rd <- readIORef (sReader s)
    case rd of
      Just (SomeThreadId tid) -> killThread tid
      Nothing -> pure ()
    tClose (sTransport s)

-- | The outcome of the 'initialize' handshake, if it has run.
negotiated :: Session -> IO (Maybe Negotiation)
negotiated s = readMVar (sNegotiated s)

--------------------------------------------------------------------------------
-- The reader loop
--------------------------------------------------------------------------------

readerLoop :: Session -> IO ()
readerLoop s = go newLineAssembler
  where
    go asm = do
      chunk <- tRecv (sTransport s)
      case chunk of
        Nothing -> drainFinal asm
        Just c -> do
          let (events, asm') = feedChunk asm c
          mapM_ (handleLine s . lineBytes) events
          go asm'
    -- EOF: a final line without its newline is still a message.
    drainFinal asm = do
      let pending = assemblerPending asm
      unless' (BS.null pending) (handleLine s pending)
    unless' b a = if b then pure () else a

lineBytes :: AssemblerEvent -> BS.ByteString
lineBytes (LineComplete l) = l

handleLine :: Session -> BS.ByteString -> IO ()
handleLine s raw =
  case A.eitherDecode' (BL.fromStrict raw) of
    Left err -> logErr ("undecodable line skipped: " <> T.pack err)
    Right v -> dispatch s (classify v)

dispatch :: Session -> Inbound -> IO ()
dispatch s inbound = case inbound of
  InboundResponse rid obj ->
    case responseKey rid of
      Just key -> do
        mcb <- modifyMVar (sPending s) (\m -> pure (IM.delete key m, IM.lookup key m))
        case mcb of
          Nothing -> logErr ("response to unknown id " <> T.pack (show key) <> " (stale or duplicate)")
          Just cb -> cb obj
      Nothing ->
        logErr "response with non-numeric id (cannot match our slots)"
  InboundRequest meth obj -> handleServerRequest s meth obj
  InboundNotification _ _ -> pure () -- lifecycle notifications; nothing to do yet
  Malformed why -> logErr ("malformed message dropped: " <> why)

-- | Answer a server-initiated request via the installed handler, or
-- refuse it. Every request gets a reply; a throwing handler becomes a
-- JSON-RPC internal error so the server never hangs on us.
handleServerRequest :: Session -> Text -> A.Object -> IO ()
handleServerRequest s meth obj = do
  mh <- readMVar (sServerHandler s)
  let rid = case KM.lookup "id" obj of
        Just v -> v
        Nothing -> A.Null
  case mh of
    Nothing -> reply s (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "error" .= errMethodNotFound])
    Just h -> do
      r <- try (h meth obj)
      case r of
        Right v -> reply s (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= v])
        Left (e :: SomeException) ->
          reply s (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid,
                           "error" .= object ["code" .= (-32603 :: Int),
                                              "message" .= T.pack (show e)]])

errMethodNotFound :: A.Value
errMethodNotFound = object ["code" .= (-32601 :: Int), "message" .= ("Method not found" :: Text)]

reply :: Session -> A.Value -> IO ()
reply s v = tSend (sTransport s) (encodeMessage v)

logErr :: Text -> IO ()
logErr t = hPutStrLn stderr ("[mcp-client] " <> T.unpack t)

--------------------------------------------------------------------------------
-- Outgoing requests
--------------------------------------------------------------------------------

-- | Send a request and wait for its response (or timeout).
--
-- The returned value is the @result@ member on success, or a
-- human-readable failure covering: session closed, deadline exceeded,
-- and the server returning a JSON-RPC error.
mcpRequest :: Session -> Text -> Maybe A.Object -> Int -> IO (Either Text A.Value)
mcpRequest s meth mparams timeoutMs = do
  st <- readIORef (sState s)
  if st == Closed
    then pure (Left "session is closed")
    else do
      i <- atomicModifyIORef' (sNextId s) (\n -> (n + 1, n + 1))
      resultVar <- newEmptyMVar
      let cb obj = putMVar resultVar obj
          req = case mparams of
            Just ps -> object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "method" .= meth, "params" .= ps]
            Nothing -> object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "method" .= meth]
      modifyMVar_ (sPending s) (\m -> pure (IM.insert i cb m))
      tSend (sTransport s) (encodeMessage req)
      -- Deadline race: whoever removes the pending entry first wins.
      -- If the reader already took it, our value is imminent.
      mres <- timeout (timeoutMs * 1000) (takeMVar resultVar)
      case mres of
        Just obj -> pure (extractResult obj)
        Nothing -> do
          removed <- modifyMVar (sPending s) (\m -> pure (IM.delete i m, IM.lookup i m))
          case removed of
            Just _ -> pure (Left ("request timed out after " <> T.pack (show timeoutMs) <> " ms"))
            -- The response raced our timeout and won the slot; its
            -- value is already in (or momentarily reaching) the MVar.
            Nothing -> do
              r2 <- try (takeMVar resultVar)
              case r2 of
                Right obj -> pure (extractResult obj)
                Left (_ :: SomeException) -> pure (Left "request abandoned")

-- | Pull @result@ or @error@ out of a full response object.
extractResult :: A.Object -> Either Text A.Value
extractResult o =
  case KM.lookup "error" o of
    Just err -> Left (describeError err)
    Nothing -> Right (maybe A.Null id (KM.lookup "result" o))
  where
    -- Render as `server error [code]: message` when the error object
    -- has the JSON-RPC shape; fall back to showing the raw value.
    describeError (A.Object eo) =
      let code = case KM.lookup "code" eo of
            Just nc -> case A.fromJSON nc of
              A.Success (i :: Int) -> T.pack (show i)
              A.Error _ -> "?"
            _ -> "?"
          msg = case KM.lookup "message" eo of
            Just (A.String m) -> m
            _ -> T.pack (show eo)
      in "server error [" <> code <> "]: " <> msg
    describeError other = "server error: " <> T.pack (show other)

-- | Send a notification (no id, no reply, no slot).
mcpNotify :: Session -> Text -> Maybe A.Object -> IO (Either Text ())
mcpNotify s meth mparams = do
  st <- readIORef (sState s)
  if st == Closed
    then pure (Left "session is closed")
    else do
      let n = case mparams of
            Just ps -> object ["jsonrpc" .= ("2.0" :: Text), "method" .= meth, "params" .= ps]
            Nothing -> object ["jsonrpc" .= ("2.0" :: Text), "method" .= meth]
      tSend (sTransport s) (encodeMessage n)
      pure (Right ())

--------------------------------------------------------------------------------
-- Handshake
--------------------------------------------------------------------------------

-- | Run the @initialize@ handshake: propose 'clientDefault', negotiate
-- against the server's reply, send @notifications\/initialized@, and
-- cache the outcome. Calling it twice fails without re-sending.
initialize :: Session -> IO (Either Text Negotiation)
initialize s = do
  -- Claim the transition atomically so concurrent/double initialize
  -- cannot both send the request.
  claimed <- atomicModifyIORef' (sState s) $ \st' -> case st' of
    Starting -> (Running, True)
    _ -> (st', False)
  if not claimed
    then pure (Left "initialize is one-shot; session already initialized or closed")
    else do
      let proposal = clientDefault
          params = case object
            [ "protocolVersion" .= revisionString proposal
            , "capabilities" .= object []
            , "clientInfo" .= object ["name" .= ("intelli-monad" :: Text), "version" .= ("0.1.3.0" :: Text)]
            ] of
            A.Object o -> o
            _ -> KM.empty -- unreachable: object always yields Object
      r <- mcpRequest s "initialize" (Just params) defaultRequestTimeoutMs
      case r of
        Left e -> pure (Left e)
        Right result -> do
          let serverVer = case result of
                A.Object ro -> case KM.lookup "protocolVersion" ro of
                  Just (A.String t) -> t
                  Just other -> T.pack (show other)
                  Nothing -> ""
                _ -> ""
              neg = negotiateWith proposal serverVer
          modifyMVar_ (sNegotiated s) (\_ -> pure (Just neg))
          _ <- mcpNotify s "notifications/initialized" Nothing
          pure (Right neg)

--------------------------------------------------------------------------------
-- Server-initiated requests
--------------------------------------------------------------------------------

-- | Install the handler for requests the server sends us
-- (@sampling\/createMessage@, @roots\/list@, @elicitation\/create@...).
-- The handler receives the method and full request object and returns
-- the @result@ value to send back. Passing 'Nothing' restores the
-- default behavior of refusing with method-not-found.
respondToServer :: Session -> Maybe (Text -> A.Object -> IO A.Value) -> IO ()
respondToServer s h = modifyMVar_ (sServerHandler s) (\_ -> pure h)
