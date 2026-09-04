{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bridge between MCP servers and louter's function-calling layer.
--
-- The REPL's tool loop is a static chain of compile-time tools
-- ('IntelliMonad.Types.ToolProxy'). This module lets every tool of
-- every connected MCP server join that chain at runtime:
--
--   * discovery: @tools\/list@ results become @['Louter.Tool']@ — the
--     shapes match exactly (name \/ description \/ inputSchema), so
--     the model sees MCP tools as first-class functions;
--   * naming: because the function-call namespace is flat, a tool is
--     encoded as @mcp__\<server\>__\<tool\>@ (server and tool names
--     containing @__@ will not round-trip through parsing — the
--     registry itself is immune, keyed by encoded name);
--   * execution: a call routed to an encoded name becomes a
--     @tools\/call@ request over the owning server's session, with
--     JSON-RPC errors mapped to 'Left'.
--
-- Nothing here touches the Prompt monad: the REPL glue (one
-- 'execMcpTool' fallback in the tool chain, one 'toLouterTool' append
-- in request assembly) stays a few lines and is not re-tested here.

module IntelliMonad.MCP.Bridge
  ( -- * Naming
    McpServerId
  , mcpToolName
  , parseMcpToolName
    -- * Discovery
  , McpToolHandle (..)
  , listMcpTools
  , toLouterTool
    -- * Registry
  , McpToolRegistry
  , newMcpToolRegistry
  , registerServer
  , registeredTools
  , execMcpTool
  , toolResultText
  , defaultListTimeoutMs
  , defaultToolCallTimeoutMs
  ) where

import Control.Concurrent.MVar
import qualified Data.Aeson as A
import Data.Aeson ((.=), object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Vector as V

import qualified Louter.Client as Louter
import qualified Louter.Types.Request as Louter

import IntelliMonad.MCP.Client
import IntelliMonad.MCP.Transport

--------------------------------------------------------------------------------
-- Naming
--------------------------------------------------------------------------------

-- | A user-chosen name for one connected MCP server.
type McpServerId = Text

-- | Encode a server-local tool into the flat function-call namespace.
mcpToolName :: McpServerId -> Text -> Text
mcpToolName server tool = "mcp__" <> server <> "__" <> tool

-- | Inverse of 'mcpToolName' (for display). Splits on the last @__@,
-- so server ids containing @__@ still parse; tool ids containing @__@
-- do not round-trip.
parseMcpToolName :: Text -> Maybe (McpServerId, Text)
parseMcpToolName t0 = do
  rest <- T.stripPrefix "mcp__" t0
  (server, tool) <- splitOnLast "__" rest
  guard' (not (T.null server) && not (T.null tool))
  pure (server, tool)
  where
    splitOnLast sep t =
      let parts = T.splitOn sep t
      in case reverse parts of
           (last' : restRev) | length parts > 1 ->
             Just (T.intercalate sep (reverse restRev), last')
           _ -> Nothing
    guard' True = Just ()
    guard' False = Nothing

--------------------------------------------------------------------------------
-- Discovery
--------------------------------------------------------------------------------

-- | One tool offered by one connected server.
data McpToolHandle = McpToolHandle
  { mthServer :: McpServerId
  , mthName :: Text -- ^ Server-local tool name.
  , mthDescription :: Maybe Text
  , mthInputSchema :: A.Value -- ^ JSON schema; {} when absent.
  } deriving (Eq, Show)

-- | Bound for @tools\/list@: discovery should be quick.
defaultListTimeoutMs :: Int
defaultListTimeoutMs = 10000

-- | Bound for @tools\/call@: tools may legitimately be slow.
defaultToolCallTimeoutMs :: Int
defaultToolCallTimeoutMs = 60000

-- | Fetch and decode a server's tool list.
listMcpTools :: Session -> McpServerId -> IO (Either Text [McpToolHandle])
listMcpTools s server = do
  r <- mcpRequest s "tools/list" Nothing defaultListTimeoutMs
  pure $ case r of
    Left e -> Left e
    Right result -> case asObject result of
      Nothing -> Left "tools/list result is not an object"
      Just o -> case KM.lookup "tools" o of
        Just (A.Array ts) -> Right (map (toHandle server) (V.toList ts))
        Just _ -> Left "tools/list result: \"tools\" is not an array"
        Nothing -> Right [] -- empty tool list
  where
    asObject (A.Object o) = Just o
    asObject _ = Nothing

toHandle :: McpServerId -> A.Value -> McpToolHandle
toHandle server v = case v of
  A.Object o -> McpToolHandle
    { mthServer = server
    , mthName = textAt "name" o
    , mthDescription = case KM.lookup "description" o of
        Just (A.String d) -> Just d
        _ -> Nothing
    , mthInputSchema = case KM.lookup "inputSchema" o of
        Just s@(A.Object _) -> s
        _ -> A.Object mempty
    }
  _ -> McpToolHandle server "" Nothing (A.Object mempty)
  where
    textAt k o = case KM.lookup k o of
      Just (A.String t) -> t
      _ -> ""

-- | Present an MCP tool to the model as a louter function.
toLouterTool :: McpToolHandle -> Louter.Tool
toLouterTool h =
  Louter.Tool
    { Louter.toolName = mcpToolName (mthServer h) (mthName h)
    , Louter.toolDescription = mthDescription h
    , Louter.toolParameters = mthInputSchema h
    }

--------------------------------------------------------------------------------
-- Registry
--------------------------------------------------------------------------------

-- | All currently connected servers and their discovered tools.
data McpToolRegistry = McpToolRegistry
  { mrSessions :: MVar (M.Map McpServerId Session)
  , mrTools :: IORef (M.Map Text McpToolHandle) -- ^ Keyed by encoded name.
  }

newMcpToolRegistry :: IO McpToolRegistry
newMcpToolRegistry = do
  ss <- newMVar M.empty
  ts <- newIORef M.empty
  pure (McpToolRegistry ss ts)

-- | Attach a session under a server id and discover its tools.
-- Re-registering the same id replaces its tools (the session itself is
-- managed by the caller). Returns the encoded tool names.
registerServer :: McpToolRegistry -> McpServerId -> Session -> IO (Either Text [Text])
registerServer reg server s = do
  etools <- listMcpTools s server
  case etools of
    Left e -> pure (Left e)
    Right handles -> do
      modifyMVar_ (mrSessions reg) (\m -> pure (M.insert server s m))
      atomicModifyIORef' (mrTools reg) $ \m ->
        ( foldl (\acc h -> M.insert (encoded h) h acc) (dropServer m) handles
        , map encoded handles
        )
      pure (Right (map encoded handles))
  where
    encoded h = mcpToolName (mthServer h) (mthName h)
    dropServer = M.filter ((/= server) . mthServer)

-- | All discovered tools, for request assembly.
registeredTools :: McpToolRegistry -> IO [McpToolHandle]
registeredTools reg = map snd . M.toList <$> readIORef (mrTools reg)

-- | Route a function call on an encoded name to the owning server.
-- The @arguments@ object is passed through unwrapped, per the MCP
-- tools\/call shape; a JSON-RPC error or transport failure becomes
-- 'Left' with the server id for context.
execMcpTool :: McpToolRegistry -> Text -> A.Object -> IO (Either Text A.Value)
execMcpTool reg encoded args = do
  tools <- readIORef (mrTools reg)
  case M.lookup encoded tools of
    Nothing -> pure (Left ("no such MCP tool: " <> encoded))
    Just h -> do
      ms <- modifyMVar (mrSessions reg) (\m -> pure (m, M.lookup (mthServer h) m))
      case ms of
        Nothing -> pure (Left ("server not connected: " <> mthServer h))
        Just s -> do
          let params = case object
                [ "name" .= mthName h
                , "arguments" .= A.Object args
                ] of
                A.Object o -> o
                _ -> KM.empty
          r <- mcpRequest s "tools/call" (Just params) defaultToolCallTimeoutMs
          pure (case r of
            Left e -> Left (mthServer h <> ": " <> e)
            Right v -> Right v)

-- | Flatten a @tools\/call@ result for a @ToolReturn@ content: the
-- joined @text@ parts of the content array when present, else the
-- encoded JSON of the whole result.
toolResultText :: A.Value -> Text
toolResultText (A.Object o) =
  case KM.lookup "content" o of
    Just (A.Array parts) ->
      let texts = [t | A.Object p <- V.toList parts
                     , Just (A.String t) <- [KM.lookup "text" p]]
      in if null texts
           then encodeUtf8' o
           else T.intercalate "\n" texts
    _ -> encodeUtf8' o
  where
    encodeUtf8' x = TE.decodeUtf8With TEE.lenientDecode (BL.toStrict (A.encode x))
toolResultText v = T.pack (show v)
