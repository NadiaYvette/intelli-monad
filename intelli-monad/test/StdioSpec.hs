{-# LANGUAGE OverloadedStrings #-}

-- | Integration test: the real stdio transport against a scripted
-- python3 MCP server. This is the wire-level acceptance test for the
-- transport contract — a real subprocess, real pipes, real EOF. It
-- reports as pending (rather than failing) when python3 is absent.

module StdioSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Timeout (timeout)

import Test.Hspec

import IntelliMonad.MCP.Client
import IntelliMonad.MCP.Negotiate
import IntelliMonad.MCP.Transport

-- | A minimal but faithful MCP server: @initialize@ (agrees on
-- 2025-06-18) and @tools/list@ (one tool); anything else is a
-- JSON-RPC error. Speaks newline-delimited JSON on stdin/stdout.
serverPy :: String
serverPy = unlines
  [ "import json, sys"
  , "for line in sys.stdin:"
  , "    line = line.strip()"
  , "    if not line:"
  , "        continue"
  , "    msg = json.loads(line)"
  , "    mid = msg.get('id')"
  , "    if mid is None:  # notification: nothing to answer"
  , "        continue"
  , "    method = msg.get('method')"
  , "    if method == 'initialize':"
  , "        print(json.dumps({'jsonrpc': '2.0', 'id': mid, 'result':"
  , "            {'protocolVersion': '2025-06-18', 'capabilities': {},"
  , "             'serverInfo': {'name': 'py-test', 'version': '0'}}}), flush=True)"
  , "    elif method == 'tools/list':"
  , "        print(json.dumps({'jsonrpc': '2.0', 'id': mid, 'result':"
  , "            {'tools': [{'name': 'echo', 'description': 'Echo',"
  , "                        'inputSchema': {'type': 'object'}}]}}), flush=True)"
  , "    elif method is not None:"
  , "        print(json.dumps({'jsonrpc': '2.0', 'id': mid, 'error':"
  , "            {'code': -32601, 'message': 'nope'}}), flush=True)"
  ]

spec :: Spec
spec = describe "stdio transport (real subprocess)" $
  it "initializes, lists tools, and shuts down a real server" $ do
    py <- findExecutable "python3"
    case py of
      Nothing -> pendingWith "python3 not available"
      Just _ -> pure ()
    trans <- openStdIO (defaultStdIOConfig "python3") { sioArgs = ["-u", "-c", serverPy] }
    s <- newSession trans
    negE <- timeout 15000000 (initialize s)
    case negE of
      Nothing -> expectationFailure "initialize timed out"
      Just (Left e) -> expectationFailure ("initialize failed: " <> T.unpack e)
      Just (Right neg) -> do
        revisionString (nAgreed neg) `shouldBe` "2025-06-18"
        r <- timeout 15000000 (mcpRequest s "tools/list" Nothing 10000)
        case r of
          Nothing -> expectationFailure "tools/list timed out"
          Just (Left e) -> expectationFailure ("tools/list failed: " <> T.unpack e)
          Just (Right res) ->
            case res of
              A.Object ro -> case KM.lookup "tools" ro of
                Just (A.Array ts) -> length ts `shouldBe` 1
                other -> expectationFailure ("unexpected result: " <> show other)
              _ -> expectationFailure ("non-object result: " <> show res)
    closeSession s
    -- A second close must be harmless (idempotence over a real
    -- process, not just the fake).
    closeSession s
