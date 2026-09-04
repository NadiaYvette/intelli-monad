{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streamable HTTP client transport (MCP spec 2025-03-26 and later).
--
-- The transport contract is the same record as stdio
-- ('IntelliMonad.MCP.Transport.Transport'): tSend / tRecv / tClose.
-- The wire differences from stdio:
--
--   * A client *request* (has an @id@) is an HTTP POST to the MCP
--     endpoint with @Accept: application/json, text/event-stream@.
--     The server may answer with a JSON body, an SSE body, or 202
--     Accepted (response deferred to the GET stream or silence).
--   * A client *notification* (no @id@) MAY get a 202 and no body;
--     we additionally keep a long-lived GET stream
--     (@Accept: text/event-stream@) open so server→client messages —
--     notifications AND server-initiated requests — arrive
--     asynchronously. Servers that do not offer GET (older 2025-03-26
--     POST-only) surface as 405, and we back off instead of hammering.
--   * The server may hand out an @Mcp-Session-Id@ response header on
--     initialize; every later request must echo it.
--
-- Incoming payloads (from POST bodies, POST SSE bodies, or GET SSE
-- bodies) are re-framed as newline-delimited JSON and pushed into an
-- internal queue; tRecv pops from that queue, so the 'Client' reader
-- loop works unchanged over this transport.
--
-- Close semantics: tClose is idempotent and stops all network
-- activity; after close, tSend is a no-op (per the transport
-- contract).
module IntelliMonad.MCP.Transport.HTTP
  ( HttpTransportConfig (..)
  , defaultHttpTransportConfig
  , openHttpTransport
  , sseDataPayloads
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import Data.List (foldl')
import qualified System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
  ( Manager,
    Request,
    RequestBody (RequestBodyLBS),
    Response,
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseTimeout,
    responseTimeoutMicro,
    responseTimeoutNone,
    responseBody,
    responseHeaders,
    responseStatus,
    setRequestIgnoreStatus,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import IntelliMonad.MCP.Transport (Transport (..))

-- | Configuration for the HTTP transport.
data HttpTransportConfig = HttpTransportConfig
  { -- | MCP endpoint, e.g. @http:\/\/127.0.0.1:8080\/mcp@.
    htcUrl :: Text,
    -- | Extra headers on every request (e.g. @Authorization@).
    htcHeaders :: [(Text, Text)],
    -- | Per-HTTP-request timeout (ms) shared by POSTs and the GET stream.
    htcTimeoutMs :: Int,
    -- | Delay (ms) before re-opening the GET stream after it ends.
    htcStreamRetryMs :: Int
  }

defaultHttpTransportConfig :: Text -> HttpTransportConfig
defaultHttpTransportConfig url =
  HttpTransportConfig
    { htcUrl = url,
      htcHeaders = [],
      htcTimeoutMs = 30000,
      htcStreamRetryMs = 500
    }

data HttpHandle = HttpHandle
  { hhManager :: Manager,
    hhConfig :: HttpTransportConfig,
    hhSessionId :: TVar (Maybe Text),
    -- ^ Server-assigned Mcp-Session-Id, captured from any response.
    hhClosed :: TVar Bool,
    hhStreamStarted :: TVar Bool,
    -- ^ Guard so the GET stream loop is forked exactly once.
    hhInbox :: TQueue BS.ByteString
    -- ^ Re-framed NDJSON lines waiting for tRecv.
  }

-- | Build a Streamable HTTP transport. The GET stream (the
-- server→client channel) starts on the first notification send,
-- which in practice is @notifications\/initialized@ right after the
-- initialize request.
openHttpTransport :: HttpTransportConfig -> IO Transport
openHttpTransport cfg = do
  mgr <- newManager tlsManagerSettings
  sid <- newTVarIO Nothing
  closed <- newTVarIO False
  started <- newTVarIO False
  inbox <- newTQueueIO
  let h = HttpHandle mgr cfg sid closed started inbox
  pure
    Transport
      { tSend = httpSend h,
        tRecv = atomically $ do
          -- Honor the transport contract: Nothing after close. The
          -- client kills its reader before closing, so the Nothing
          -- path is rarely observed — but it must exist.
          c <- readTVar closed
          if c then pure Nothing else Just <$> readTQueue inbox,
        tClose = atomically (writeTVar closed True)
        -- In-flight HTTP calls finish and land in the queue harmlessly;
        -- the stream loop checks 'hhClosed' between attempts.
      }

--------------------------------------------------------------------------------
-- Sending
--------------------------------------------------------------------------------

-- | Classify the outgoing frame: a request (has @id@) is a POST; a
-- notification triggers the long-lived GET stream (once).
httpSend :: HttpHandle -> BS.ByteString -> IO ()
httpSend h payload = do
  isClosed <- readTVarIO (hhClosed h)
  unless isClosed $
    case A.decode' (BL.fromStrict payload) of
      Just v@(A.Object o) ->
        case KM.lookup "id" o of
          Just _ -> void (forkIO (httpPost h v))
          Nothing -> ensureGetStream h
      _ -> pure () -- undecodable frame: drop (never produced internally)

-- | POST one request message. The response body — plain JSON or an
-- SSE stream — is fed to the inbox. Runs in a worker thread; failures
-- are logged to stderr (never thrown — a dead server surfaces as a
-- request timeout or EOF fail-fast in the client).
httpPost :: HttpHandle -> A.Value -> IO ()
httpPost h v = do
  eresp <- try (buildRequest h "POST" (Just v) >>= \req -> httpLbs req (hhManager h))
  case eresp of
    Left (e :: SomeException) -> logHttp ("POST failed: " <> T.pack (show e))
    Right resp -> do
      captureSessionId h resp
      let body = responseBody resp
      when (statusCode (responseStatus resp) == 200) $
        if isSseBody resp
          then mapM_ (feedInbox h) (sseDataPayloads body)
          else feedInbox h body

logHttp :: Text -> IO ()
logHttp t = BC.hPutStrLn System.IO.stderr ("[mcp-http] " <> T.encodeUtf8 t)

-- | The long-lived server→client stream. Re-opens with backoff until
-- the transport closes. 405 means the server offers no GET stream
-- (2025-03-26 POST-only servers); back off without error — responses
-- for those servers come on POST bodies only.
httpGetStreamLoop :: HttpHandle -> IO ()
httpGetStreamLoop h = go
  where
    go = do
      isClosed <- readTVarIO (hhClosed h)
      if isClosed
        then pure ()
        else do
          eresp <- try (buildRequest h "GET" Nothing >>= \req -> httpLbs req (hhManager h))
          case eresp of
            Left (_ :: SomeException) -> void (retryAfter 2000)
            Right resp ->
              case statusCode (responseStatus resp) of
                405 -> void (retryAfter 5000) -- no GET stream support; poll gently
                200 ->
                  if isSseBody resp
                    then do
                      mapM_ (feedInbox h) (sseDataPayloads (responseBody resp))
                      retryAfter (htcStreamRetryMs (hhConfig h))
                    else retryAfter (htcStreamRetryMs (hhConfig h))
                _ -> retryAfter (htcStreamRetryMs (hhConfig h))
    retryAfter ms = threadDelay (ms * 1000) >> go

ensureGetStream :: HttpHandle -> IO ()
ensureGetStream h = do
  started <- atomically $ do
    s <- readTVar (hhStreamStarted h)
    when (not s) (writeTVar (hhStreamStarted h) True)
    pure s
  unless started (void (forkIO (httpGetStreamLoop h)))

-- | Common request assembly: URL, Accept/Content-Type, session id,
-- user headers, JSON body for POSTs, ignore-status (we interpret),
-- per-kind response timeout (POSTs bounded by config; the GET stream
-- unbounded — it returns when the server closes the stream).
buildRequest :: HttpHandle -> BS.ByteString -> Maybe A.Value -> IO Request
buildRequest h method mbody = do
  req0 <- parseRequest (T.unpack (htcUrl (hhConfig h)))
  sid <- readTVarIO (hhSessionId h)
  let headers =
        [ (CI.mk "Accept", "application/json, text/event-stream"),
          (CI.mk "Content-Type", "application/json")
        ]
          ++ maybe [] (\s -> [(CI.mk "Mcp-Session-Id", T.encodeUtf8 s)]) sid
          ++ [(CI.mk (T.encodeUtf8 k), T.encodeUtf8 v) | (k, v) <- htcHeaders (hhConfig h)]
      req1 = req0 {requestHeaders = headers}
      req2 = case mbody of
        Just v | method == "POST" -> req1 {requestBody = RequestBodyLBS (A.encode v)}
        _ -> req1
      req3 =
        req2
          { method = method,
            responseTimeout =
              if method == "POST"
                then responseTimeoutMicro (htcTimeoutMs (hhConfig h) * 1000)
                else responseTimeoutNone
          }
  pure (setRequestIgnoreStatus req3)

captureSessionId :: HttpHandle -> Response BL.ByteString -> IO ()
captureSessionId h resp =
  case lookupHeader "Mcp-Session-Id" (responseHeaders resp) of
    Just v -> atomically (writeTVar (hhSessionId h) (Just (T.decodeUtf8Lenient v)))
    Nothing -> pure ()

-- | Case-insensitive header lookup over response headers.
lookupHeader :: BS.ByteString -> [(CI.CI BS.ByteString, BS.ByteString)] -> Maybe BS.ByteString
lookupHeader name =
  foldl'
    ( \acc (k, v) ->
        if acc == Nothing && CI.mk name == k
          then Just v
          else acc
    )
    Nothing

isSseBody :: Response BL.ByteString -> Bool
isSseBody resp =
  case lookupHeader "Content-Type" (responseHeaders resp) of
    Just ct -> "text/event-stream" `BC.isInfixOf` ct
    Nothing -> False

-- | Push one payload into the inbox as an NDJSON frame (adds the
-- newline). Blank/whitespace-only payloads are dropped — SSE
-- keep-alives must not become spurious empty frames.
feedInbox :: HttpHandle -> BL.ByteString -> IO ()
feedInbox h payload
  | BL.all (\w -> w == 32 || w == 9 || w == 13 || w == 10) payload = pure ()
  | otherwise = atomically (writeTQueue (hhInbox h) (BL.toStrict payload <> "\n"))

--------------------------------------------------------------------------------
-- SSE parsing
--------------------------------------------------------------------------------

-- | Extract the joined @data:@ payload of every SSE event in a body.
-- Consecutive @data:@ lines of one event join with @\n@; a blank line
-- ends the event; comments and other fields are ignored. CR is
-- stripped (SSE treats CRLF, CR and LF alike). Exported for tests.
sseDataPayloads :: BL.ByteString -> [BL.ByteString]
sseDataPayloads body =
  [joinedEv ev | ev <- events, hasData ev]
  where
    events = splitEvents (map stripCR (BC.lines (BL.toStrict body)))
    hasData = any ("data:" `BC.isPrefixOf`)
    dataLines ev = [BS.drop 5 l | l <- ev, "data:" `BC.isPrefixOf` l]
    joinedEv ev = BL.fromStrict (BS.intercalate "\n" (map stripLeadingSpace (dataLines ev)))
    stripLeadingSpace l = case BC.uncons l of
      Just (' ', rest) -> rest
      _ -> l

-- SSE normalizes CRLF and CR line endings to LF.
stripCR :: BS.ByteString -> BS.ByteString
stripCR l = if "\r" `BC.isSuffixOf` l then BS.init l else l

-- Split one SSE body into events (lists of lines, order preserved);
-- blank lines separate events.
splitEvents :: [BS.ByteString] -> [[BS.ByteString]]
splitEvents = go []
  where
    go acc [] = [reverse acc | not (null acc)]
    go acc (l : rest)
      | BS.null l = case acc of
          [] -> go [] rest
          _ -> reverse acc : go [] rest
      | otherwise = go (l : acc) rest
