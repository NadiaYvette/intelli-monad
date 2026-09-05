{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | DPella cross-check (Phase 2 acceptance): our MCP client against the
-- @mcp@ package's example server
-- (<https://github.com/dpella/mcp>), a real Haskell MCP server built
-- on Servant + @servant-auth@ JWT.
--
-- This is the external check the plan calls for: two independent
-- Haskell implementations meeting on the transport they uniquely
-- share — Streamable HTTP. Wire facts verified by curl before
-- encoding:
--
--   * the example server listens on @$PORT\/mcp@ and prints a fresh
--     JWT to stdout (line-buffered, so a pipe works);
--   * @POST \/mcp@ answers @200@ with an SSE body whose data lines
--     have @data:@ with NO space after the colon (their MimeRender);
--     our SSE parser accepts both spellings;
--   * it negotiates @2025-06-18@ (the equal path, like our own
--     server; the pty acceptance covered negotiate-down);
--   * @Authorization: Bearer <jwt>@ is required; a bad token gets a
--     bare @401@ — the one server we accept against that exercises
--     the transport's HTTP-level fail-fast path (the pending request
--     must fail immediately with a synthetic error, not hang);
--   * tools: @echo@ (text result), @add@ (structured content).
--
-- Prerequisites: the DPella repo cloned at \/tmp\/dpella-mcp and the
-- example built on ghc-9.10.3 (their cabal.project pins ghc-9.12; on
-- this machine the freeze file was moved aside so the solver could
-- pick 9.10-compatible versions). Override the binary with
-- @IM_DPELLA_EXAMPLE@. When absent, the spec pends — it is an
-- acceptance test of an external artifact, not of this repo.
module DpellaSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, unless, void)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest)
import qualified Network.Socket as NS
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Environment (getEnvironment)
import System.IO (IOMode (WriteMode), openFile)
import System.Process
  ( CreateProcess (env, std_err, std_out),
    StdStream (CreatePipe, UseHandle),
    proc,
    terminateProcess,
    waitForProcess,
    withCreateProcess,
  )
import System.Timeout (timeout)

import Test.Hspec

import IntelliMonad.MCP.Client
import IntelliMonad.MCP.Negotiate
import IntelliMonad.MCP.Transport.HTTP

-- | Candidate example-server binaries: env override first, then the
-- known build-tree path.
candidateBinaries :: IO [FilePath]
candidateBinaries = do
  envOverride <- lookupEnv "IM_DPELLA_EXAMPLE"
  let built =
        "/tmp/dpella-mcp/dist-newstyle/build/x86_64-linux/ghc-9.10.3/"
          <> "mcp-example-0.1.0.0/x/mcp-example/opt/build/mcp-example/mcp-example"
  existing <- filterM doesFileExist (maybe [] (: []) envOverride ++ [built])
  pure existing

-- Free port allocation (same trick as HttpSpec).
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

-- | The Bearer token is the first non-empty line after the banner.
extractToken :: [Text] -> Maybe Text
extractToken ls =
  case dropWhile (not . ("Bearer token for testing:" `T.isPrefixOf`)) ls of
    (_ : rest) -> case dropWhile T.null rest of
      (tok : _) -> Just (T.strip tok)
      [] -> Nothing
    [] -> Nothing

-- | Wait until the HTTP server accepts connections (any response
-- counts — even 404 proves Warp is listening).
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

-- | Start the example server and hand the (port, JWT) to the body.
-- Logs to \/tmp\/dpella-server.log for post-mortem.
withExampleServer :: ((Int, Text) -> IO ()) -> IO ()
withExampleServer body = do
  cands <- candidateBinaries
  case cands of
    [] -> pure () -- caller pends in this case
    (bin : _) -> do
      port <- getFreePort
      outH <- openFile "/tmp/dpella-server.log" WriteMode
      env0 <- getEnvironment
      -- The example reads its port from $PORT (default 8080). REPLACE
      -- it, never append: this machine's login env exports PORT=20128,
      -- and a child that sees two PORTs honors the first (execve
      -- semantics) — the appended value is silently shadowed. Learned
      -- the hard way: the server bound 20128 (OmniRoute's port, busy)
      -- and the spec polled a port nothing ever listened on.
      let childEnv = ("PORT", show port) : [kv | kv <- env0, fst kv /= "PORT"]
      withCreateProcess
        (proc bin [])
          { std_out = CreatePipe,
            std_err = UseHandle outH,
            env = Just childEnv
          }
        $ \_ mout _ ph -> do
          ready <- waitHttpReady port 60
          unless ready $
            fail
              ( "dpella example server did not come up on port "
                  <> show port
                  <> " (see /tmp/dpella-server.log)"
              )
          -- stdout is line-buffered; give the banner a moment, then
          -- read what's there (the process stays alive, so no EOF).
          threadDelay 1000000
          mtoken <- case mout of
            Nothing -> pure Nothing
            Just h -> extractToken . T.lines . TE.decodeUtf8 <$> BS.hGetSome h 65536
          case mtoken of
            Nothing -> fail "could not read JWT from example server stdout (see /tmp/dpella-server.log)"
            Just tok -> body (port, tok)
          terminateProcess ph
          void (waitForProcess ph)

spec :: Spec
spec = describe "DPella cross-check (their server, our client)" $
  it "JWT handshake, SSE-wrapped calls, 401 fail-fast" $ do
    cands <- candidateBinaries
    case cands of
      [] -> pendingWith "dpella/mcp example not built at /tmp/dpella-mcp (see module header)"
      (_ : _) -> withExampleServer (\(port, tok) -> do
        let url = "http://127.0.0.1:" <> T.pack (show port) <> "/mcp"

        -- 1. Happy path: full session with the real JWT.
        trans <-
          openHttpTransport
            (defaultHttpTransportConfig url) {htcHeaders = [("Authorization", "Bearer " <> tok)]}
        s <- newSession trans
        negE <- timeout 20000000 (initialize s)
        case negE of
          Nothing -> expectationFailure "initialize timed out"
          Just (Left e) -> expectationFailure ("initialize failed: " <> T.unpack e)
          -- They negotiate the exact version we propose: the equal
          -- path (our pty acceptance covered negotiate-down).
          Just (Right neg) -> revisionString (nAgreed neg) `shouldBe` "2025-06-18"

        -- Discovery over an SSE-wrapped POST: proves our parser eats
        -- their no-space `data:` framing.
        tl <- timeout 20000000 (mcpRequest s "tools/list" Nothing 15000)
        case tl of
          Nothing -> expectationFailure "tools/list timed out"
          Just (Left e) -> expectationFailure ("tools/list failed: " <> T.unpack e)
          Just (Right (A.Object o)) ->
            case KM.lookup "tools" o of
              Just (A.Array xs) ->
                let names =
                      [ n
                      | A.Object t <- toList xs
                      , Just (A.String n) <- [KM.lookup "name" t]
                      ]
                 in sort names `shouldBe` ["add", "current-time", "echo"]
              _ -> expectationFailure "tools missing from reply"
          Just (Right v) -> expectationFailure ("unexpected tools/list reply: " <> show v)

        -- Their echo tool round-trips text through the whole stack.
        call <- timeout 20000000 $
          mcpRequest
            s
            "tools/call"
            ( Just $
                KM.fromList
                  [ ("name", A.String "echo"),
                    ("arguments", A.object [("message", A.String "cross-check-ok")])
                  ]
            )
            15000
        case call of
          Nothing -> expectationFailure "tools/call timed out"
          Just (Left e) -> expectationFailure ("tools/call failed: " <> T.unpack e)
          Just (Right (A.Object o)) ->
            case KM.lookup "content" o of
              Just (A.Array xs) ->
                let texts =
                      [ t
                      | A.Object c <- toList xs
                      , Just (A.String t) <- [KM.lookup "text" c]
                      ]
                 in texts `shouldSatisfy` elem "cross-check-ok"
              _ -> expectationFailure ("tools/call reply missing content: " <> show o)
          Just (Right v) -> expectationFailure ("unexpected tools/call reply: " <> show v)

        closeSession s

        -- 2. The 401 fail-fast path: with a garbage token the server
        -- answers bare HTTP 401. The pending initialize must fail
        -- fast with the synthetic transport error. The outer
        -- 10s timeout is the fail-fast assertion: a regression to
        -- hang-until-request-timeout turns this into Nothing.
        badTrans <-
          openHttpTransport
            (defaultHttpTransportConfig url) {htcHeaders = [("Authorization", "Bearer garbage")]}
        sBad <- newSession badTrans
        r401 <- timeout 10000000 (initialize sBad)
        case r401 of
          Nothing -> expectationFailure "401 path: initialize hung (fail-fast broken)"
          Just Right {} -> expectationFailure "401 path: initialize unexpectedly succeeded"
          Just (Left e) -> e `shouldSatisfy` ("HTTP transport error" `T.isInfixOf`))
