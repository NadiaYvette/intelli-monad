{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The typed wire layer: DPella's @mcp-types@ (2025-06-18 protocol
-- types) bridged onto our raw-aeson session machinery.
--
-- 'IntelliMonad.MCP.Client' and 'IntelliMonad.MCP.Server' speak aeson
-- 'A.Value' end to end — deliberately, so the session machinery never
-- breaks on a schema the typed layer cannot represent (see the lossy
-- note below). This module is the /typed/ entrance for callers who
-- want the 2025-06-18 data model:
--
--   * 'encodeRequest' \/ 'decodeResult' — build a typed request into
--     the raw wire object the session machinery sends, and pull a
--     typed result back out of the captured @result@ member;
--   * 'pairMessage' — classify one inbound message with the
--     @jsonrpc@ package's 'JSONRPCMessage' model (the typed mirror of
--     "IntelliMonad.MCP.Correlate"'s hand-rolled classification);
--   * server-side helpers — 'typedTool' (descriptor as
--     @MCP.Types.'Tool'@), 'decodeCallToolRequest' (validated
--     params), 'callToolResult' (spec-shaped success envelope),
--     'callToolError' (typed error body), 'typedInitializeResult'
--     (typed handshake result).
--
-- The typed vocabulary itself is NOT re-exported here — @MCP.Protocol@
-- and @MCP.Types@ share record-field names (e.g. @role@, @name@), so a
-- blanket re-export of both is ambiguous. Import them alongside:
--
-- > import IntelliMonad.MCP.Wire (decodeCallToolRequest, callToolResult)
-- > import MCP.Protocol (CallToolRequest (..))
-- > import MCP.Types (Tool (..))
--
-- == Why the client discovery path stays raw
--
-- @MCP.Types.InputSchema@ models exactly @{type, properties, required}@
-- — any other schema keyword (@additionalProperties@, @$defs@, …) is
-- dropped on decode. Server-side that is lossless: our generated
-- 'IT.JSONSchema' output is exactly that three-key shape. On the
-- client side it would silently discard foreign servers' richer
-- schemas, so "IntelliMonad.MCP.Bridge" keeps raw 'A.Value' schemas.
-- The trade is explicit: server round-trips are typed end to end;
-- client discovery of /foreign/ schemas stays raw.
--
-- License note: @mcp-types@ is MPL-2.0 — used as a dependency only;
-- none of its source is copied into this MIT-licensed tree.
module IntelliMonad.MCP.Wire
  ( -- * Typed requests
    encodeRequest
  , decodeResult
    -- * Message classification (typed)
  , pairMessage
  , inboundFromPair
    -- * Server-side typed surface
  , typedInitializeResult
  , typedTool
  , toolFromProxy
  , decodeCallToolRequest
  , callToolResult
  , callToolError
  ) where

import Data.Aeson ((.=), FromJSON, ToJSON (..), Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Proxy (..))
import qualified Data.Vector as V

import MCP.Protocol
-- RequestId is re-exported by both modules (Protocol re-exports the
-- jsonrpc package's, Types re-exports it as a basic type); one copy.
import MCP.Types hiding (RequestId)

import qualified IntelliMonad.Types as IT

import IntelliMonad.MCP.Correlate (Inbound (..))

--------------------------------------------------------------------------------
-- Typed requests
--------------------------------------------------------------------------------

-- | Build a typed request into the raw wire object the session
-- machinery sends. The id travels inside the typed record; the method
-- name comes from the 'IsJSONRPCRequest' instance. A @params@ of
-- 'Null' (from a @Maybe@-less or 'Nothing' params field) is omitted
-- from the wire object, mirroring the raw path.
--
-- >>> import qualified Data.Aeson as A
-- >>> import qualified Data.Aeson.KeyMap as KM
-- >>> let req = ListToolsRequest (RequestId (A.Number 7)) Nothing
-- >>> KM.lookup "method" (encodeRequest req)
-- Just (String "tools/list")
-- >>> KM.lookup "params" (encodeRequest req)
-- Nothing
-- >>> KM.lookup "id" (encodeRequest req)
-- Just (Number 7.0)
encodeRequest :: forall req. (IsJSONRPCRequest req) => req -> A.Object
encodeRequest req = case toJSONRPCRequest req of
  -- JSONRPCRequest { jsonrpc, id, method, params } — positional to
  -- avoid the duplicated record-field names.
  JSONRPCRequest _ rid meth p -> KM.fromList (withParams rid meth p)
  where
    withParams rid meth p =
      let base =
            [ "jsonrpc" .= rPC_VERSION
            , "id" .= rid
            , "method" .= meth
            ]
       in case p of
            Null -> base
            _ -> base ++ ["params" .= p]

-- | Pull a typed result out of the raw @result@ member the session
-- machinery captured ('IntelliMonad.MCP.Client.mcpRequest' returns
-- exactly that member). The response id is matched by the correlation
-- layer before this sees the value.
--
-- >>> import qualified Data.Aeson as A
-- >>> :{
--   let raw = A.object
--         [ ("tools", A.toJSON ([A.object [("name", A.String "w"), ("inputSchema", A.object [("type", A.String "object")])]] :: [A.Value]))
--         , ("nextCursor", A.Null)
--         , ("_meta", A.Null)
--         ]
--    in case decodeResult raw :: Either Text ListToolsResult of
--         Right (ListToolsResult ts _ _) -> Right (length ts)
--         Left e -> Left e
-- :}
-- Right 1
decodeResult :: forall res. (FromJSON res) => A.Value -> Either Text res
decodeResult v = case A.fromJSON v of
  AT.Success r -> Right r
  AT.Error err -> Left (T.pack err)

--------------------------------------------------------------------------------
-- Message classification (typed)
--------------------------------------------------------------------------------

-- | Classify one inbound message with the @jsonrpc@ package's model —
-- the typed mirror of "IntelliMonad.MCP.Correlate" 'Inbound'
-- classification. Used by callers who want typed access to the
-- envelope; the session machinery itself keeps its hand-rolled
-- classifier (it doubles as the fallback for messages the typed model
-- rejects).
--
-- >>> import qualified Data.Aeson as A
-- >>> let n = A.object [("jsonrpc", A.String "2.0"), ("method", A.String "notifications/initialized")]
-- >>> fmap (\m -> case m of NotificationMessage{} -> True; _ -> False) (pairMessage n)
-- Right True
pairMessage :: A.Value -> Either Text JSONRPCMessage
pairMessage v = case A.fromJSON v of
  AT.Success m -> Right m
  AT.Error err -> Left (T.pack err)

-- | The typed mirror of 'Inbound': 'Just' for message kinds the
-- session machinery also recognizes, 'Nothing' for typed-model-only
-- rejections. Useful for cross-checking the two classifiers in tests.
-- The carried object is the full re-encoded message, matching
-- 'Inbound''s contract (method + full message, full message for
-- result\/error extraction).
inboundFromPair :: JSONRPCMessage -> Maybe Inbound
inboundFromPair m =
  let objOf v = case v of
        A.Object o -> o
        _ -> KM.empty
   in case m of
        RequestMessage r@(JSONRPCRequest _ _ meth _) ->
          Just (InboundRequest meth (objOf (toJSON r)))
        ResponseMessage r@(JSONRPCResponse _ rid _) ->
          Just (InboundResponse (requestIdValue rid) (objOf (toJSON r)))
        ErrorMessage e@(JSONRPCError _ rid _) ->
          Just (InboundResponse (requestIdValue rid) (objOf (toJSON e)))
        NotificationMessage n@(JSONRPCNotification _ meth _) ->
          Just (InboundNotification meth (objOf (toJSON n)))
  where
    requestIdValue (RequestId v') = v'

--------------------------------------------------------------------------------
-- Server-side typed surface
--------------------------------------------------------------------------------

-- | Build the @initialize@ result from typed records — the typed
-- route for what the raw path assembled by hand. Emits exactly
-- @{protocolVersion, capabilities, serverInfo}@ (optional members
-- omitted by @mcp-types@' @omitNothingFields@).
--
-- >>> import qualified Data.Aeson as A
-- >>> import qualified Data.Aeson.KeyMap as KM
-- >>> let A.Object km = typedInitializeResult "2025-06-18" "intelli-monad" "0.1.3.0"
-- >>> KM.member "capabilities" km
-- True
typedInitializeResult :: Text -> Text -> Text -> A.Value
typedInitializeResult protoVersion serverName serverVersion =
  toJSON
    ( InitializeResult
        protoVersion
        (ServerCapabilities Nothing Nothing Nothing (Just (ToolsCapability (Just False))) Nothing Nothing)
        (Implementation serverName serverVersion Nothing)
        Nothing
        Nothing
    )

-- | The @MCP.Types.'Tool'@ descriptor for one of our compile-time
-- tools. Our generated schemas are @{type, properties, required}@ —
-- exactly what @MCP.Types.'InputSchema'@ models — so this conversion
-- is lossless for our own tools.
--
-- Field order (constructed positionally to avoid the duplicated
-- record-field names across @mcp-types@):
-- @name, title, description, inputSchema, outputSchema, annotations, _meta@.
typedTool :: forall t. (IT.Tool t, IT.JSONSchema t, IT.HasFunctionObject t) => Proxy t -> Tool
typedTool _ =
  Tool
    (IT.toolFunctionName @t)
    Nothing
    (Just (T.pack (IT.getFunctionDescription @t)))
    (typedInputSchema (IT.toAeson (IT.schema @t)))
    Nothing
    Nothing
    Nothing

-- | The raw wire descriptor for one tool — what @tools/list@ serves.
-- Equal to @toJSON . typedTool@; kept as a named function so the
-- server's call sites read the same in both worlds.
toolFromProxy :: (IT.Tool t, IT.JSONSchema t, IT.HasFunctionObject t) => Proxy t -> A.Value
toolFromProxy p = toJSON (typedTool p)

-- | Interpret a schema value as an @MCP.Types.'InputSchema'@. The
-- three modeled keys are extracted; everything else is dropped (the
-- documented lossy trade — see the module header).
typedInputSchema :: A.Value -> InputSchema
typedInputSchema v = case v of
  A.Object o ->
    InputSchema
      (fromMaybe "object" (keyText "type" o))
      (case KM.lookup "properties" o of
         Just ps@(A.Object _) -> case A.fromJSON ps :: AT.Result (Map Text Value) of
           AT.Success mp -> Just mp
           _ -> Nothing
         _ -> Nothing)
      (case KM.lookup "required" o of
         Just (A.Array reqs) -> Just [t | String t <- V.toList reqs]
         _ -> Nothing)
  _ -> InputSchema "object" Nothing Nothing
  where
    keyText k o = case KM.lookup k o of
      Just (String t) -> Just t
      _ -> Nothing

-- | Decode and validate a @tools\/call@ request from the full inbound
-- message object. Returns the raw request id (needed for the
-- response), the tool name, and the arguments object (missing
-- @arguments@ becomes the empty object, per the raw path's behavior).
-- This is the validation the raw path did by hand with @KM.lookup@s.
decodeCallToolRequest :: A.Value -> Either Text (A.Value, Text, A.Object)
decodeCallToolRequest v = case A.fromJSON v of
  AT.Success (CallToolRequest (RequestId rid) (CallToolParams nm margs)) ->
    let asObj val = case val of
          A.Object o -> o
          _ -> KM.empty
     in Right
          ( rid
          , nm
          , maybe (KM.empty) (asObj . A.toJSON) margs
          )
  AT.Error err -> Left (T.pack err)

-- | The spec-shaped success envelope for a @tools\/call@: one text
-- content block plus optional structured content. Tool failures
-- belong in the result's @isError@, /not/ in a JSON-RPC error — the
-- LLM must see them to self-correct.
--
-- Field order: @content, structuredContent, isError, _meta@.
--
-- >>> import qualified Data.Aeson as A
-- >>> import qualified Data.Aeson.KeyMap as KM
-- >>> let A.Object km = callToolResult "ok" Nothing
-- >>> KM.member "content" km
-- True
callToolResult :: Text -> Maybe (Map Text Value) -> A.Value
callToolResult txt structured =
  -- TextContent: textType, text, annotations, _meta
  -- CallToolResult: content, structuredContent, isError, _meta
  toJSON
    ( CallToolResult
        [TextBlock (TextContent "text" txt Nothing Nothing)]
        structured
        Nothing
        Nothing
    )

-- | A typed JSON-RPC error body (the @error@ member of a response).
callToolError :: Int -> Text -> JSONRPCErrorInfo
callToolError errCode errMsg = JSONRPCErrorInfo errCode errMsg Nothing
