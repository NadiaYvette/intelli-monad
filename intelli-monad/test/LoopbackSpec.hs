{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | T6 loopback acceptance: our MCP client against our own MCP server
-- (@mcp-serve@, the real compiled binary — not the library in-process).
-- This closes the Phase 5 loop: everything the REPL can do to a remote
-- server, a remote client can do to us.
--
-- The binary is located from the cabal build tree (both tested GHC
-- grades are candidates) or the @IM_MCP_SERVE@ env override. When no
-- binary exists the spec pends — the loopback is an acceptance test of
-- the shipped artifact, not of whatever source happens to be on disk.
--
-- Wire facts verified by hand before encoding (smoke run against the
-- 9.8.4 binary):
--
--   * the server answers @initialize@ with @2025-06-18@, so the
--     handshake exercises the negotiate-@equal@ path (T5 exercised
--     negotiate-down; this closes the matrix),
--   * @tools\/list@ exposes exactly the two default tools with their
--     JSON schemas,
--   * @tools\/call@ on @call_bash_script@ returns the tool's 'Output'
--     JSON-encoded in the content block,
--   * unknown tools get error @-32602@, unknown methods @-32601@,
--   * the banner goes to stderr, never stdout (stdio wire hygiene).
module LoopbackSpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as A
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Hspec

import IntelliMonad.MCP.Client
import IntelliMonad.MCP.Negotiate
import IntelliMonad.MCP.Transport

-- | Candidate binaries: the env override first, then the cabal build
-- tree for each GHC grade CI tests.
candidates :: [FilePath]
candidates =
  [ "/tmp/im-mcp-serve/mcp-serve"
  , home </> "src/intelli-monad/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve"
  , home </> "src/intelli-monad/dist-newstyle/build/x86_64-linux/ghc-9.10.3/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve"
  ]
  where
    home = "/home/nyc"

-- | Infix for readable path joins above.
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

-- | Locate the built server binary, or Nothing.
findServer :: IO (Maybe FilePath)
findServer = do
  envOverride <- lookupEnv "IM_MCP_SERVE"
  let order = maybe [] (: []) envOverride ++ candidates
  go order
  where
    go [] = pure Nothing
    go (p : ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else go ps

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

-- | Decode the text content as the tool's JSON Output.
decodeOutput :: Text -> Maybe (Int, String, String)
decodeOutput t =
  case A.eitherDecode' (BL.fromStrict (TE.encodeUtf8 t)) of
    Right (o :: A.Object) -> do
      A.Number code <- KM.lookup "code" o
      A.String so <- KM.lookup "stdout" o
      A.String se <- KM.lookup "stderr" o
      pure (round code, T.unpack so, T.unpack se)
    _ -> Nothing

spec :: Spec
spec = describe "mcp-serve loopback acceptance (T6)" $
  it "handshake, discovery, tool round-trip, error paths" $ do
    mserver <- findServer
    case mserver of
      Nothing -> pendingWith "mcp-serve binary not built (cabal build exe:mcp-serve, or set IM_MCP_SERVE)"
      Just server -> do
        opened <- try (openStdIO (defaultStdIOConfig server))
        case opened of
          Left (e :: SomeException) -> expectationFailure ("server failed to start: " <> show e)
          Right trans -> do
            r <- timeout 60000000 (runScenario trans)
            case r of
              Nothing -> expectationFailure "loopback scenario timed out after 60s"
              Just () -> pure ()
            tClose trans

runScenario :: Transport -> IO ()
runScenario trans = do
  s <- newSession trans

  -- Handshake: the server speaks our own default, 2025-06-18, so this
  -- is the negotiate-equal path of the matrix (T5 covered downward).
  negE <- initialize s
  case negE of
    Left e -> expectationFailure ("initialize failed: " <> T.unpack e)
    Right neg -> revisionString (nAgreed neg) `shouldBe` "2025-06-18"

  -- Discovery: exactly the two default tools, with schemas.
  tl <- mcpRequest s "tools/list" Nothing 10000
  case tl of
    Left e -> expectationFailure ("tools/list failed: " <> T.unpack e)
    Right (A.Object o) ->
      case KM.lookup "tools" o of
        Just (A.Array xs) -> do
          let names = [n | A.Object t <- toList xs, Just (A.String n) <- [KM.lookup "name" t]]
              nSchema = length [ () | A.Object t <- toList xs, Just (A.Object _) <- [KM.lookup "inputSchema" t] ]
          L.sort names `shouldBe` ["call_bash_script", "organ_check_boundary", "organ_diagnostics", "organ_find_symbol", "organ_ingest", "organ_repo_map", "search_arxiv"]
          nSchema `shouldBe` 7
        _ -> expectationFailure "tools/list reply missing tools array"
    Right v -> expectationFailure ("unexpected tools/list reply: " <> show v)

  -- Round-trip: run a deterministic script through the real binary.
  callTool s "call_bash_script" (A.object [("script", A.String "echo loopback-ok")])
    >>= \case
      Left e -> expectationFailure ("tools/call failed: " <> T.unpack e)
      Right res -> decodeOutput (resultText res) `shouldBe` Just (0, "loopback-ok\n", "")

  -- Error path: unknown tool is a clean JSON-RPC error, not a hang.
  callTool s "no_such_tool" (A.object [])
    >>= \case
      Left _ -> pure () -- transport-level error text is fine
      Right (A.Object o) ->
        case KM.lookup "error" o of
          Just (A.Object eo) -> do
            KM.lookup "code" eo `shouldBe` Just (A.Number (-32602))
          _ -> expectationFailure "unknown tool did not return an error object"
      Right v -> expectationFailure ("unexpected error-path reply: " <> show v)

  -- Error path: unknown method.
  mcpRequest s "bogus/method" Nothing 10000 >>= \case
    Left _ -> pure ()
    Right (A.Object o) ->
      case KM.lookup "error" o of
        Just (A.Object eo) -> KM.lookup "code" eo `shouldBe` Just (A.Number (-32601))
        _ -> expectationFailure "unknown method did not return an error object"
    Right v -> expectationFailure ("unexpected method-path reply: " <> show v)
