{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | T5 acceptance: the MCP client against the real @pty-mcp-server@
-- application. This is the wire-level acceptance target that echo
-- servers can never provide: a stateful server with multi-turn
-- process sessions, non-blocking reads (including the documented
-- empty-string case), server-enforced command whitelisting, and a
-- real process lifecycle (spawn, reuse, terminate, respawn).
--
-- The server is sandboxed per run: its own temp tree for logs, its
-- own @tools-list.json@ declaring only the four @agent-proc@ tools,
-- and a config whose @agentAllowedCmds@ whitelist permits only @cat@
-- (plus bash, which the server needs internally to be useful). The
-- spec respects the project's own caution banner: nothing runs
-- unsupervised and nothing leaves the sandbox tree.
--
-- @cat@ is deliberate: with no file argument it copies stdin to
-- stdout line-buffered and stays alive — a stateful, interactive,
-- non-TUI process, exactly what @agent-proc-run@ documents. bash is
-- reserved for human-supervised sessions; a test should not depend
-- on shell state.
--
-- Facts established against pty-mcp-server 0.2.2.0 (all verified by
-- hand before being encoded here):
--
--   * invoked as @pty-mcp-server --yaml FILE@ (the README's positional
--     form does not work; the parser rejects it),
--   * the config yaml requires every key: logDir, logLevel, toolsDir,
--     promptsDir, resourcesDir, sandboxDir, prompts, invalidChars,
--     invalidCmds, invalidPatterns, agentAllowedCmds, sandboxNetworks —
--     a missing key aborts startup with an AesonException (which is
--     why a server dying at startup must fail its clients fast; that
--     client-side fix lives in 'IntelliMonad.MCP.Client'),
--   * all paths are resolved against the server process's CWD, so the
--     spec writes absolute paths,
--   * it answers @2024-11-05@ — older than our proposal — so this test
--     exercises the negotiate-*downward* path of the matrix,
--   * tool results are text content; @cat@ echoes include the trailing
--     newline.
module PtySpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson ((.=), object)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.FilePath ((</>))
import Control.Concurrent (threadDelay)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)

import Test.Hspec

import IntelliMonad.MCP.Client
import IntelliMonad.MCP.Negotiate
import IntelliMonad.MCP.Transport

-- | The four agent-proc tools, in the server's own tools-list.json
-- schema (name/description/inputSchema). The server refuses to expose
-- any tool that is not declared in this file.
toolsListJson :: String
toolsListJson = unlines
  [ "["
  , "{\"name\":\"agent-proc-run\",\"description\":\"Spawn external process.\","
      <> "\"inputSchema\":{\"type\":\"object\",\"properties\":"
      <> "{\"command\":{\"type\":\"string\"},"
      <> "\"arguments\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},"
      <> "\"environment\":{\"type\":\"object\",\"additionalProperties\":{\"type\":\"string\"}}},"
      <> "\"required\":[\"command\"]}},"
  , "{\"name\":\"agent-proc-read\",\"description\":\"Non-blocking read; empty string if no data.\","
      <> "\"inputSchema\":{\"type\":\"object\",\"properties\":"
      <> "{\"arguments\":{\"type\":\"integer\",\"description\":\"Max bytes.\"}},"
      <> "\"required\":[\"arguments\"]}},"
  , "{\"name\":\"agent-proc-write\",\"description\":\"Write to process stdin.\","
      <> "\"inputSchema\":{\"type\":\"object\",\"properties\":"
      <> "{\"data\":{\"type\":\"string\"},"
      <> "\"appendNewline\":{\"type\":\"boolean\"}},"
      <> "\"required\":[\"data\"]}},"
  , "{\"name\":\"agent-proc-terminate\",\"description\":\"Kill the running process.\","
      <> "\"inputSchema\":{\"type\":\"object\"}}"
  , "]"
  ]

-- | Sandbox config. Paths must be ABSOLUTE: they are interpreted
-- relative to the server process's working directory (inherited from
-- the test runner), not the sandbox root. Every key is required by
-- the server's config parser.
configYaml :: FilePath -> String
configYaml root = unlines
  [ "logDir : \"" <> root </> "logs\""
  , "logLevel : \"Info\""
  , "toolsDir : \"" <> root </> "tools\""
  , "promptsDir : \"" <> root </> "prompts\""
  , "resourcesDir : \"" <> root </> "resources\""
  , "sandboxDir : \"" <> root </> "sandbox\""
  , "prompts:"
  , "- 'ghci>'"
  , "- ']$'"
  , "- 'Password:'"
  , "invalidChars:"
  , "- '&&'"
  , "- '||'"
  , "invalidCmds:"
  , "- rm"
  , "- sudo"
  , "invalidPatterns:"
  , "- 'rm\\s'"
  , "agentAllowedCmds:"
  , "- cat"
  , "- bash"
  , "- /bin/bash"
  , "sandboxNetworks:"
  , "- 127.0.0.0/8"
  ]

-- | Lay out the sandbox tree the server requires and point the config
-- at absolute paths inside it.
buildSandbox :: FilePath -> IO FilePath
buildSandbox root = do
  mapM_ (createDirectoryIfMissing True . (root </>))
    [ "logs", "tools", "prompts", "resources", "sandbox" ]
  writeFile (root </> "pty-mcp-server.yaml") (configYaml root)
  writeFile (root </> "tools" </> "tools-list.json") toolsListJson
  pure (root </> "pty-mcp-server.yaml")

-- | Locate the installed server binary, or Nothing.
findServer :: IO (Maybe FilePath)
findServer = do
  let prebuilt = "/tmp/pty-install-test/pty-mcp-server"
  ok <- doesFileExist prebuilt
  if ok then pure (Just prebuilt) else findExecutable "pty-mcp-server"

-- | Call a tool; result is the unwrapped result object.
callTool :: Session -> Text -> A.Value -> IO (Either Text A.Value)
callTool s name args =
  mcpRequest s "tools/call"
    (Just (KM.fromList [("name", A.String name), ("arguments", args)])) 10000

-- | Flatten an MCP tool result to its text content.
resultText :: A.Value -> Text
resultText (A.Object o) =
  case KM.lookup "content" o of
    Just (A.Array xs) -> T.intercalate "\n"
      [ t | A.Object c <- toList xs, Just (A.String t) <- [KM.lookup "text" c] ]
    _ -> ""
resultText _ = ""

spec :: Spec
spec = describe "pty-mcp-server acceptance (T5)" $
  it "drives a stateful process session end-to-end" $ do
    mserver <- findServer
    case mserver of
      Nothing -> pendingWith "pty-mcp-server not installed (cabal install pty-mcp-server)"
      Just server -> withSystemTempDirectory "ptymcp-t5" $ \root -> do
        cfg <- buildSandbox root
        started <- try $ openStdIO (defaultStdIOConfig server) { sioArgs = ["--yaml", cfg] }
        case started of
          Left (e :: SomeException) -> expectationFailure ("server failed to start: " <> show e)
          Right trans -> do
            r <- timeout 120000000 $ runScenario trans
            case r of
              Nothing -> expectationFailure "scenario timed out after 120s"
              Just () -> pure ()
            tClose trans
            tClose trans  -- idempotent over a real subprocess

runScenario :: Transport -> IO ()
runScenario trans = do
  s <- newSession trans

  -- Handshake: the server answers 2024-11-05, so the matrix must
  -- negotiate us DOWN from the 2025-06-18 proposal.
  negE <- initialize s
  case negE of
    Left e -> expectationFailure ("initialize failed: " <> T.unpack e)
    Right neg -> revisionString (nAgreed neg) `shouldBe` "2024-11-05"

  -- Discovery: exactly the four tools we declared.
  tl <- mcpRequest s "tools/list" Nothing 10000
  case tl of
    Left e -> expectationFailure ("tools/list failed: " <> T.unpack e)
    Right (A.Object o) ->
      case KM.lookup "tools" o of
        Just (A.Array ts) ->
          mapM_ (`shouldSatisfy` toolPresent ts) ["agent-proc-run", "agent-proc-read", "agent-proc-write", "agent-proc-terminate"]
        _ -> expectationFailure "tools/list result missing tools array"
    Right other -> expectationFailure ("unexpected tools/list result: " <> show other)

  -- ---- Scenario A: a stateful session with cat ----------------------
  spawn1 <- callTool s "agent-proc-run" (object ["command" .= ("cat" :: Text)])
  case spawn1 of
    Left e -> expectationFailure ("agent-proc-run failed: " <> T.unpack e)
    Right _ -> pure ()

  -- Non-blocking read before any input: documented empty-string case.
  -- (Whitespace is stripped: the server emits a startup newline on
  -- session begin, which is noise, not data.)
  read0 <- callTool s "agent-proc-read" (object ["arguments" .= (1024 :: Int)])
  case read0 of
    Left e -> expectationFailure ("first read should not error: " <> T.unpack e)
    Right v -> T.strip (resultText v) `shouldBe` ""

  -- Write and read back: the stateful round-trip echo servers fake.
  w1 <- callTool s "agent-proc-write" (object ["data" .= ("hello-pty\n" :: Text)])
  case w1 of
    Left e -> expectationFailure ("write failed: " <> T.unpack e)
    Right _ -> pure ()
  r1 <- pollText s 1024 "hello-pty"
  r1 `shouldBe` "hello-pty"

  -- Second read: still nothing pending (empty, not error) — proves
  -- the read is non-blocking and the earlier read consumed the line.
  r2 <- callTool s "agent-proc-read" (object ["arguments" .= (1024 :: Int)])
  case r2 of
    Left e -> expectationFailure ("second read should not error: " <> T.unpack e)
    Right v -> T.strip (resultText v) `shouldBe` ""

  -- Terminate, then verify the state actually reset by spawning again.
  t1 <- callTool s "agent-proc-terminate" (object [])
  case t1 of
    Left e -> expectationFailure ("terminate failed: " <> T.unpack e)
    Right _ -> pure ()
  spawn2 <- callTool s "agent-proc-run" (object ["command" .= ("cat" :: Text)])
  case spawn2 of
    Left e -> expectationFailure ("respawn after terminate failed: " <> T.unpack e)
    Right _ -> pure ()
  w2 <- callTool s "agent-proc-write" (object ["data" .= ("second-round\n" :: Text)])
  case w2 of
    Left e -> expectationFailure ("write after respawn failed: " <> T.unpack e)
    Right _ -> pure ()
  r3 <- pollText s 1024 "second-round"
  r3 `shouldBe` "second-round"
  _ <- callTool s "agent-proc-terminate" (object [])  -- leave it clean

  -- ---- Scenario B: the whitelist is enforced by the server ---------
  denied <- callTool s "agent-proc-run" (object ["command" .= ("whoami" :: Text)])
  case denied of
    Right v ->
      -- The server surfaces denial as a normal result whose text says so.
      resultText v `shouldSatisfy` \t ->
        any (`T.isInfixOf` T.toLower t) ["not allowed", "denied"]
    Left e -> e `shouldSatisfy` T.isInfixOf "server error"

  closeSession s

  -- ---- Scenario C: server death fails fast --------------------------
  -- A fresh request on the closed session must return Left
  -- immediately (session-closed), never hang.
  after <- mcpRequest s "tools/list" Nothing 3000
  case after of
    Left _ -> pure ()
    Right _ -> expectationFailure "request after close unexpectedly succeeded"
  where
    toolPresent ts name = name `elem` names
      where
        names = [ n | A.Object t <- toList ts, Just (A.String n) <- [KM.lookup "name" t] ]

-- | Poll until the expected text appears (up to ~2s) or return the
-- last read. cat is line-buffered; a single read can race its echo.
pollText :: Session -> Int -> Text -> IO Text
pollText s n expect = go (10 :: Int)
  where
    go 0 = pure ""
    go k = do
      r <- callTool s "agent-proc-read" (object ["arguments" .= n])
      case r of
        Left e -> fail ("read failed: " <> T.unpack e)
        Right v -> do
          let t = resultText v
          if expect `T.isInfixOf` t
            then pure (T.strip t)
            else do
              threadDelay 200000
              go (k - 1)
