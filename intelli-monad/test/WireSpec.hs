{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the typed wire layer ("IntelliMonad.MCP.Wire").
--
-- The layer's contract has two halves:
--
--  1. /Faithful to the wire/ — typed encode and decode produce exactly
--     what the raw path produces (verified here against the raw
--     fixtures the other specs pin, not a parallel dialect).
--  2. /Faithful to the typed model/ — 'decodeCallToolRequest'
--     validates with mcp-types' own parser, so malformed params are
--     rejected the same way across every client.
module WireSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import IntelliMonad.MCP.Correlate (Inbound (..), classify)
import IntelliMonad.MCP.Wire
import qualified IntelliMonad.Tools.Bash as Bash
import JSONRPC (JSONRPCErrorInfo (..), JSONRPCMessage (..), JSONRPCNotification (..), JSONRPCRequest (..), RequestId (..))
import MCP.Protocol (CallToolParams (..), CallToolRequest (..), ListToolsRequest (..), ListToolsResult (..), PingRequest (..))
import qualified MCP.Types (InputSchema (..), Tool (..))

spec :: Spec
spec = do
  describe "encodeRequest" $ do
    it "tools/list without params: method, id, no params key" $ do
      let req = ListToolsRequest (RequestId (A.Number 7)) Nothing
          o = encodeRequest req
      KM.lookup "method" o `shouldBe` Just (A.String "tools/list")
      KM.lookup "id" o `shouldBe` Just (A.Number 7)
      KM.lookup "params" o `shouldBe` Nothing

    it "produces the same object the raw path builds (modulo key order)" $ do
      -- The raw path's fixture, from Bridge/Client conventions.
      let raw = A.object
            [ "jsonrpc" A..= ("2.0" :: Text)
            , "id" A..= (7 :: Int)
            , "method" A..= ("tools/list" :: Text)
            ]
          typed = encodeRequest (ListToolsRequest (RequestId (A.Number 7)) Nothing)
      -- Key order is unspecified in aeson; compare as values.
      (A.toJSON typed :: A.Value) `shouldBe` raw

    it "tools/call: arguments travel as a params object" $ do
      let req = CallToolRequest (RequestId (A.Number 3))
                  (CallToolParams "call_bash_script" (Just (M.fromList [("script" :: Text, A.toJSON ("echo hi" :: Text))])))
          o = encodeRequest req
      KM.lookup "method" o `shouldBe` Just (A.String "tools/call")
      case KM.lookup "params" o of
        Just (A.Object p) -> do
          KM.lookup "name" p `shouldBe` Just (A.String "call_bash_script")
          case KM.lookup "arguments" p of
            Just (A.Object a) -> KM.lookup "script" a `shouldBe` Just (A.String "echo hi")
            other -> expectationFailure ("arguments not an object: " <> show other)
        other -> expectationFailure ("params not an object: " <> show other)

    it "ping with Nothing params omits params key" $ do
      let req = PingRequest (RequestId (A.String "abc")) Nothing
          o = encodeRequest req
      KM.lookup "method" o `shouldBe` Just (A.String "ping")
      KM.lookup "params" o `shouldBe` Nothing

  describe "decodeResult" $ do
    it "tools/list result decodes to typed ListToolsResult" $ do
      let raw = A.object
            [ "tools" A..= [ A.object
                [ "name" A..= ("call_bash_script" :: Text)
                , "description" A..= ("Call a bash script" :: Text)
                , "inputSchema" A..= A.object
                    [ "type" A..= ("object" :: Text)
                    , "properties" A..= (A.object [] :: A.Value)
                    , "required" A..= ([] :: [Text])
                    ]
                ] ]
            , "nextCursor" A..= A.Null
            , "_meta" A..= A.Null
            ]
      case decodeResult raw :: Either Text ListToolsResult of
        Right (ListToolsResult ts _ _) -> length ts `shouldBe` 1
        Left e -> expectationFailure (T.unpack e)

    it "rejects a malformed result with a field-naming error" $
      case decodeResult (A.String "nope") :: Either Text ListToolsResult of
        Left _ -> pure ()
        Right _ -> expectationFailure "should not decode"

  describe "pairMessage / inboundFromPair (cross-check vs Correlate.classify)" $ do
    it "response classifies identically in both models" $ do
      let v = A.object
            [ "jsonrpc" A..= ("2.0" :: Text)
            , "id" A..= (1 :: Int)
            , "result" A..= A.object []
            ]
      case (classify v, pairMessage v) of
        (InboundResponse{}, Right (ResponseMessage _)) -> pure ()
        (other, p) -> expectationFailure ("mismatch: " <> show other <> " / " <> show p)

    it "server notification classifies identically in both models" $ do
      let v = A.object
            [ "jsonrpc" A..= ("2.0" :: Text)
            , "method" A..= ("notifications/initialized" :: Text)
            ]
      case (classify v, pairMessage v) of
        (InboundNotification meth _, Right (NotificationMessage (JSONRPCNotification _ m _)))
          | meth == m -> pure ()
        (other, p) -> expectationFailure ("mismatch: " <> show other <> " / " <> show p)

    it "server-initiated request classifies identically in both models" $ do
      let v = A.object
            [ "jsonrpc" A..= ("2.0" :: Text)
            , "id" A..= (9 :: Int)
            , "method" A..= ("roots/list" :: Text)
            ]
      case (classify v, pairMessage v) of
        (InboundRequest meth _, Right (RequestMessage (JSONRPCRequest _ _ m _)))
          | meth == m -> pure ()
        (other, p) -> expectationFailure ("mismatch: " <> show other <> " / " <> show p)

  describe "typedTool / toolFromProxy (Bash as the concrete tool)" $ do
    it "descriptor carries the Bash tool's name and schema" $ do
      let d = typedTool (Proxy @Bash.Bash)
      MCP.Types.name d `shouldBe` "call_bash_script"
      MCP.Types.schemaType (MCP.Types.inputSchema d) `shouldBe` "object"

    it "raw wire form matches the raw path's descriptor exactly" $ do
      let raw = A.object
            [ "name" A..= ("call_bash_script" :: Text)
            , "description" A..= ("Call a bash script in a local environment" :: Text)
            , "inputSchema" A..= A.object
                [ "type" A..= ("object" :: Text)
                , "properties" A..= A.object
                    [ "script" A..= A.object
                        [ "type" A..= ("string" :: Text)
                        , "description" A..= ("A script executing in a local environment" :: Text)
                        ]
                    ]
                , "required" A..= (["script" :: Text])
                ]
            ]
          typed = toolFromProxy (Proxy @Bash.Bash)
      typed `shouldBe` raw

    it "typed and raw descriptors encode identically (A.toJSON . typedTool)" $
      (A.toJSON (typedTool (Proxy @Bash.Bash)) :: A.Value)
        `shouldBe` toolFromProxy (Proxy @Bash.Bash)

  describe "decodeCallToolRequest" $ do
    it "round-trips what encodeRequest builds" $ do
      let req = CallToolRequest (RequestId (A.Number 5))
                  (CallToolParams "call_bash_script" (Just (M.fromList [("script" :: Text, A.toJSON ("ls" :: Text))])))
          wire = A.toJSON (encodeRequest req)
      case decodeCallToolRequest wire of
        Right (rid, nm, args) -> do
          rid `shouldBe` A.Number 5
          nm `shouldBe` "call_bash_script"
          KM.lookup "script" args `shouldBe` Just (A.String "ls")
        Left e -> expectationFailure (T.unpack e)

    it "missing arguments becomes the empty object" $ do
      let wire = A.object
            [ "jsonrpc" A..= ("2.0" :: Text)
            , "id" A..= (5 :: Int)
            , "method" A..= ("tools/call" :: Text)
            , "params" A..= A.object [("name" A..= ("call_bash_script" :: Text))]
            ]
      case decodeCallToolRequest wire of
        Right (_, _, args) -> (KM.null args) `shouldBe` True
        Left e -> expectationFailure (T.unpack e)

    it "rejects a message with the wrong method tag" $ do
      let wire = A.object
            [ "jsonrpc" A..= ("2.0" :: Text)
            , "id" A..= (5 :: Int)
            , "method" A..= ("ping" :: Text)  -- typed CallToolRequest requires tools/call
            , "params" A..= A.object [("name" A..= ("call_bash_script" :: Text))]
            ]
      case decodeCallToolRequest wire of
        Left _ -> pure ()
        Right _ -> expectationFailure "wrong-method message must not decode"

  describe "callToolResult" $ do
    it "emits the spec envelope (content array, one text block)" $ do
      let v = callToolResult "hello" Nothing
      case v of
        A.Object o -> do
          KM.lookup "content" o `shouldBe` Just
            (A.toJSON [A.object ["type" A..= ("text" :: Text), "text" A..= ("hello" :: Text)]])
          KM.member "structuredContent" o `shouldBe` False
          KM.member "isError" o `shouldBe` False
        other -> expectationFailure ("not an object: " <> show other)

  describe "callToolError" $ do
    it "carries code and message" $
      callToolError (-32602) "bad params" `shouldBe`
        JSONRPCErrorInfo (-32602) "bad params" Nothing
