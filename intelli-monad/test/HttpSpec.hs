{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase 2 transport tests: the Streamable HTTP client transport.
--
-- Two layers:
--
--   * pure tests of the SSE event parser ('sseDataPayloads') —
--     multi-line data, single-line data, comments, other fields,
--     CRLF, keep-alives;
--   * an integration suite against a scripted python3 MCP server
--     that speaks Streamable HTTP for real: POST returns a JSON
--     response and assigns an @Mcp-Session-Id@ (which later POSTs
--     must echo); a @tools\/list@ response comes back wrapped in
--     SSE; a GET @text/event-stream@ delivers a server-initiated
--     notification.
module HttpSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest)
import qualified Network.Socket as NS
import System.Directory (findExecutable)
import System.IO (openFile, IOMode (WriteMode))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
  ( CreateProcess (std_err),
    StdStream (UseHandle),
    proc,
    terminateProcess,
    waitForProcess,
    withCreateProcess,
  )
import System.Timeout (timeout)

import Test.Hspec

import IntelliMonad.MCP.Client
import IntelliMonad.MCP.Negotiate
import IntelliMonad.MCP.Transport
import IntelliMonad.MCP.Transport.HTTP

-- Free port allocation: bind port 0, read the assigned port, close.
getFreePort :: IO Int
getFreePort = do
  addr : _ <- NS.getAddrInfo
    (Just NS.defaultHints {NS.addrSocketType = NS.Stream})
    (Just "127.0.0.1")
    (Just "0")
  sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
  NS.bind sock (NS.addrAddress addr)
  NS.getSocketName sock >>= \case
    NS.SockAddrInet p _ -> NS.close sock >> pure (fromIntegral p)
    _ -> NS.close sock >> fail "unexpected socket address"

-- The Streamable HTTP test server. Complete MCP flow in ~100 lines of
-- python; HTTP/1.0 close-delimited responses keep it deterministic.
serverPy :: Int -> String
serverPy port = unlines
  [ "import json, threading, time"
  , "from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer"
  , ""
  , "SESSION = [None]"
  , ""
  , "def sse(events):"
  , "    out = []"
  , "    for i, data in enumerate(events):"
  , "        out.append('event: message')"
  , "        out.append('id: ' + str(i))"
  , "        for ln in data.split('\\n'):"
  , "            out.append('data: ' + ln)"
  , "        out.append('')"
  , "    return ('text/event-stream', '\\n'.join(out).encode())"
  , ""
  , "def json_body(v):"
  , "    return ('application/json', json.dumps(v).encode())"
  , ""
  , "class H(BaseHTTPRequestHandler):"
  , "    protocol_version = 'HTTP/1.0'  # close-delimited; simple + robust"
  , "    def log_message(self, *a):"
  , "        pass"
  , "    def _send(self, code, ctype, body):"
  , "        self.send_response(code)"
  , "        self.send_header('Content-Type', ctype)"
  , "        self.send_header('Content-Length', str(len(body)))"
  , "        self.end_headers()"
  , "        self.wfile.write(body)"
  , "    def do_POST(self):"
  , "        n = int(self.headers.get('Content-Length', 0))"
  , "        msg = json.loads(self.rfile.read(n) or b'{}')"
  , "        sid = self.headers.get('Mcp-Session-Id')"
  , "        mid = msg.get('id')"
  , "        if msg.get('method') == 'initialize':"
  , "            SESSION[0] = 'sess-123'"
  , "            resp = {'jsonrpc': '2.0', 'id': mid, 'result':"
  , "                    {'protocolVersion': '2025-06-18', 'capabilities': {},"
  , "                     'serverInfo': {'name': 'py-http', 'version': '0'}}}"
  , "            ct, body = json_body(resp)"
  , "            self.send_response(200)"
  , "            self.send_header('Content-Type', ct)"
  , "            self.send_header('Mcp-Session-Id', 'sess-123')"
  , "            self.send_header('Content-Length', str(len(body)))"
  , "            self.end_headers()"
  , "            self.wfile.write(body)"
  , "        elif sid != 'sess-123':"
  , "            self._send(400, 'application/json', b'{\"error\": \"bad session\"}')"
  , "        elif msg.get('method') == 'tools/list':"
  , "            resp = {'jsonrpc': '2.0', 'id': mid, 'result':"
  , "                    {'tools': [{'name': 'ping', 'description': 'Ping',"
  , "                                'inputSchema': {'type': 'object'}}]}}"
  , "            ct, body = sse([json.dumps(resp)])"
  , "            self.send_response(200)"
  , "            self.send_header('Content-Type', ct)"
  , "            self.send_header('Content-Length', str(len(body)))"
  , "            self.end_headers()"
  , "            self.wfile.write(body)"
  , "        else:"
  , "            self._send(200, 'application/json',"
  , "                       json.dumps({'jsonrpc': '2.0', 'id': mid,"
  , "                                   'result': {}}).encode())"
  , "    def do_GET(self):"
  , "        sid = self.headers.get('Mcp-Session-Id')"
  , "        if sid != 'sess-123':"
  , "            self._send(400, 'text/plain', b'bad session')"
  , "            return"
  , "        notify = json.dumps({'jsonrpc': '2.0', 'method':"
  , "                             'notifications/message',"
  , "                             'params': {'level': 'info',"
  , "                                        'data': 'stream-alive'}})"
  , "        ct, body = sse([notify])"
  , "        self.send_response(200)"
  , "        self.send_header('Content-Type', ct)"
  , "        self.send_header('Content-Length', str(len(body)))"
  , "        self.end_headers()"
  , "        self.wfile.write(body)"
  , ""
  , "srv = ThreadingHTTPServer(('127.0.0.1', " <> show port <> "), H)"
  , "threading.Thread(target=srv.serve_forever, daemon=True).start()"
  , "time.sleep(60)  # parent kills us; keep serving meanwhile"
  , ""
  ]

spec :: Spec
spec = do
  describe "SSE parser (pure)" $ do
    it "joins multi-line data fields with newlines" $ do
      let body = "data: {\"a\":\ndata: 1}\n\n"
      sseDataPayloads body `shouldBe` ["{\"a\":\n1}"]
    it "separates events on blank lines and ignores comments/fields" $ do
      let body = ": keep-alive\nevent: message\nid: 7\ndata: one\n\ndata: two\n"
      sseDataPayloads body `shouldBe` ["one", "two"]
    it "strips exactly one leading space per data line" $ do
      let body = "data:  spaced\n\n"
      sseDataPayloads body `shouldBe` [" spaced"]
    it "handles CRLF line endings and empty bodies" $ do
      sseDataPayloads "data: a\r\n\r\ndata: b\r\n\r\n" `shouldBe` ["a", "b"]
      sseDataPayloads "" `shouldBe` []

  describe "streamable HTTP transport (real subprocess)" $
    it "initializes, lists tools over SSE-wrapped POST, and picks up the session id" $ do
      py <- findExecutable "python3"
      case py of
        Nothing -> pendingWith "python3 not available"
        Just _ -> pure ()
      port <- getFreePort
      withSystemTempDirectory "mcp-http" $ \tmp -> do
        let logPath = "/tmp/mcp-http-server.log"
        logH <- openFile logPath WriteMode
        withCreateProcess
          (proc "python3" ["-u", "-c", serverPy port]) {std_err = UseHandle logH}
          $ \_ _ _ ph -> do
            ready <- waitHttpReady port 40
            unless ready $ fail "test HTTP server did not come up"
            trans <-
              openHttpTransport
                (defaultHttpTransportConfig ("http://127.0.0.1:" <> T.pack (show port) <> "/mcp"))
            s <- newSession trans
            negE <- timeout 15000000 (initialize s)
            case negE of
              Nothing -> expectationFailure "initialize timed out"
              Just (Left e) -> expectationFailure ("initialize failed: " <> T.unpack e)
              Just (Right neg) -> revisionString (nAgreed neg) `shouldBe` "2025-06-18"
            -- tools/list returns wrapped in SSE over POST. The server
            -- 400s any POST without the session id, so success here
            -- also proves Mcp-Session-Id capture + echo.
            tl <- timeout 15000000 (mcpRequest s "tools/list" Nothing 10000)
            case tl of
              Nothing -> expectationFailure "tools/list timed out"
              Just (Left e) -> expectationFailure ("tools/list failed: " <> T.unpack e)
              Just (Right res) ->
                case res of
                  A.Object o ->
                    case KM.lookup "tools" o of
                      Just (A.Array ts) -> length ts `shouldBe` 1
                      _ -> expectationFailure "tools missing"
                  _ -> expectationFailure ("unexpected result: " <> show res)
            closeSession s
            terminateProcess ph
            _ <- waitForProcess ph
            pure ()

-- Wait until the HTTP server accepts connections (any response counts).
waitHttpReady :: Int -> Int -> IO Bool
waitHttpReady port tries = go tries
  where
    go 0 = pure False
    go n = do
      r <- try $ do
        req <- parseRequest ("http://127.0.0.1:" <> show port <> "/health")
        mgr <- newManager defaultManagerSettings
        _ <- httpLbs req mgr
        pure ()
      case r of
        Right () -> pure True
        Left (_ :: SomeException) -> threadDelay 250000 >> go (n - 1)
