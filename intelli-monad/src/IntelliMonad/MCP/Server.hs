{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Expose louter's compile-time tools over MCP: the *server* half of
-- the bridge. Where "IntelliMonad.MCP.Bridge" pulls remote MCP tools
-- into the REPL's function-calling chain, this module pushes local
-- 'IntelliMonad.Types.ToolProxy' tools out to any MCP client.
--
-- A tool descriptor is derivable entirely from a 'ToolProxy': the
-- existential constraint bundle carries the name
-- ('IntelliMonad.Types.toolFunctionName'), the JSON input schema
-- ('IntelliMonad.Types.schema' via 'IntelliMonad.Types.JSONSchema'),
-- and enough to decode arguments and run the tool. The one design
-- decision from the plan — which session a @tools\/call@ binds to —
-- is answered with a 'StatelessConf'-style execution: the server runs
-- in a fresh 'StateT PromptEnv' environment per call with the session
-- name supplied at bind time, so the server holds no session state
-- and every call is independent.
--
-- The wire surface is the stdio subset of the 2025-06-18 server
-- spec: @initialize@, @notifications\/initialized@, @ping@,
-- @tools\/list@, @tools\/call@. Streamable HTTP serving is future
-- work; the method dispatch itself is transport-agnostic.
module IntelliMonad.MCP.Server
  ( ServeConfig (..)
  , defaultServeConfig
  , serveTools
  , handleCall
  , toolDescriptor
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson ((.=), object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (Day (..), UTCTime (..))
import Data.Typeable (Proxy (..))
import System.IO (BufferMode (..), hFlush, hIsEOF, hSetBuffering, stderr, stdin, stdout)

import IntelliMonad.MCP.Framing (encodeMessage, newLineAssembler, LineAssembler, assemblerPending, feedChunk, AssemblerEvent (..))
import IntelliMonad.MCP.Transport (Transport (..))
import IntelliMonad.Persist (StatelessConf (..))
import IntelliMonad.Types

-- | Configuration for a serving instance.
data ServeConfig = ServeConfig
  { scTools :: [ToolProxy]
  -- ^ Tools to expose (usually 'IntelliMonad.Tools.defaultTools').
  , scSessionName :: Text
  -- ^ Session name bound to incoming @tools\/call@s.
  , scBanner :: Bool
  -- ^ Print the caution banner on startup (upstream convention).
  }

-- | A serve config exposing no tools under the @mcp-serve@ session
-- name, with the startup banner enabled.
defaultServeConfig :: ServeConfig
defaultServeConfig =
  ServeConfig
    { scTools = [],
      scSessionName = "mcp-serve",
      scBanner = True
    }

--------------------------------------------------------------------------------
-- Tool descriptors: ToolProxy -> MCP tools/list entry
--------------------------------------------------------------------------------

-- | Build the MCP tool descriptor (name \/ description \/ schema) for
-- one compile-time tool.
toolDescriptor :: ToolProxy -> A.Value
toolDescriptor (ToolProxy (_ :: Proxy t)) =
  object
    [ "name" .= toolFunctionName @t,
      "description" .= T.pack (getFunctionDescription @t),
      "inputSchema" .= toAeson (schema @t)
    ]

--------------------------------------------------------------------------------
-- Execution: tools/call -> toolExec in a fresh stateless environment
--------------------------------------------------------------------------------

-- | Execute a tool call in a fresh 'StatelessConf' environment. The
-- result text is the JSON encoding of the tool's 'Output', mirroring
-- how 'IntelliMonad.Types.ToolReturn' stores it for the LLM.
handleCall ::
  forall t.
  (Tool t, A.FromJSON t, A.ToJSON (Output t), JSONSchema t, HasFunctionObject t) =>
  ServeConfig ->
  Text ->
  A.Value ->
  IO (Either A.Value A.Value)
handleCall cfg argText args =
  case (A.fromJSON args :: A.Result t) of
    A.Error err -> pure (Left (toolError (-32602) ("invalid arguments for " <> argText <> ": " <> T.pack err)))
    A.Success input -> do
      let env0 = initialPromptEnv (scSessionName cfg)
          act :: StateT PromptEnv IO (Output t)
          act = toolExec @t @StatelessConf @IO input
      r <- try (evalStateT act env0)
      case r of
        Left (e :: SomeException) ->
          pure (Left (toolError (-32603) (T.pack (show e))))
        Right out -> do
          let outText :: Text
              outText = T.decodeUtf8Lenient (BL.toStrict (A.encode out))
          pure (Right (object ["content" .= [object ["type" .= ("text" :: Text), "text" .= outText]]]))

-- | A JSON-RPC error value for tool failures.
toolError :: Int -> Text -> A.Value
toolError code msg =
  object ["code" .= code, "message" .= msg]

-- | The minimal environment 'toolExec' needs: it only touches the
-- session name (for logging contents) and the backend (persisting
-- happens on the stateless backend, i.e. nowhere).
initialPromptEnv :: Text -> PromptEnv
initialPromptEnv sessionName =
  PromptEnv
    { tools = [],
      customInstructions = [],
      context =
        Context
          { contextRequest = fromModel "",
            contextResponse = Nothing,
            contextHeader = [],
            contextBody = [],
            contextFooter = [],
            contextTotalTokens = 0,
            contextSessionName = sessionName,
            contextCreated = UTCTime (ModifiedJulianDay 0) 0
          },
      backend = PersistProxy StatelessConf,
      hooks = [],
      timeoutSeconds = Nothing
    }

--------------------------------------------------------------------------------
-- Method dispatch
--------------------------------------------------------------------------------

-- | Handle one inbound JSON-RPC message; produce the outbound value
-- (a JSON-RPC response object) or Nothing for notifications.
handleCall' :: ServeConfig -> A.Value -> IO (Maybe A.Value)
handleCall' cfg msg = case msg of
  A.Object o -> do
    let mid = KM.lookup "id" o
        meth = case KM.lookup "method" o of
          Just (A.String m) -> Just m
          _ -> Nothing
    case (mid, meth) of
      -- Notification: nothing to answer.
      (Nothing, _) -> pure Nothing
      (Just _, Nothing) -> pure Nothing
      (Just mid', Just meth') -> Just <$> dispatch cfg mid' meth' o
  _ -> pure Nothing

dispatch :: ServeConfig -> A.Value -> Text -> A.Object -> IO A.Value
dispatch cfg mid meth o = case meth of
  "initialize" ->
    pure $
      object
        [ "jsonrpc" .= ("2.0" :: Text),
          "id" .= mid,
          "result"
            .= object
              [ "protocolVersion" .= ("2025-06-18" :: Text),
                "capabilities" .= object ["tools" .= object ["listChanged" .= False]],
                "serverInfo"
                  .= object
                    ["name" .= ("intelli-monad" :: Text), "version" .= ("0.1.3.0" :: Text)]
              ]
        ]
  "ping" ->
    pure $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= mid, "result" .= object []]
  "tools/list" ->
    pure $
      object
        [ "jsonrpc" .= ("2.0" :: Text),
          "id" .= mid,
          "result" .= object ["tools" .= map toolDescriptor (scTools cfg)]
        ]
  "tools/call" -> do
    let params = case KM.lookup "params" o of
          Just (A.Object p) -> p
          _ -> KM.empty
        name = case KM.lookup "name" params of
          Just (A.String n) -> n
          _ -> ""
        args = maybe (A.Object KM.empty) id (KM.lookup "arguments" params)
    r <- runTool cfg name args
    pure $ case r of
      Right result ->
        object ["jsonrpc" .= ("2.0" :: Text), "id" .= mid, "result" .= result]
      Left err ->
        object ["jsonrpc" .= ("2.0" :: Text), "id" .= mid, "error" .= err]
  _ ->
    pure $
      object
        ["jsonrpc" .= ("2.0" :: Text), "id" .= mid, "error" .= toolError (-32601) ("method not found: " <> meth)]

runTool :: ServeConfig -> Text -> A.Value -> IO (Either A.Value A.Value)
runTool cfg name args = go (scTools cfg)
  where
    go [] = pure (Left (toolError (-32602) ("tool not found: " <> name)))
    go (p : ps) = case p of
      tp@(ToolProxy (_ :: Proxy t)) ->
        if toolFunctionName @t == name
          then handleCall @t cfg name args
          else go ps

--------------------------------------------------------------------------------
-- stdio serving
--------------------------------------------------------------------------------

-- | Serve the configured tools over stdio until stdin closes. Errors
-- never kill the loop: an inbound line that fails to parse is logged
-- to stderr and dropped; a handler exception becomes a JSON-RPC
-- internal error response (via the Except-style result in dispatch).
serveTools :: ServeConfig -> IO ()
serveTools cfg = do
  -- The banner goes to stderr: stdout is the JSON-RPC wire for the
  -- stdio transport, and any banner byte there corrupts the protocol
  -- stream for clients that do not tolerate chatter between frames.
  when (scBanner cfg) $
    mapM_
      (BC.hPutStrLn stderr)
      [ "-----------------------------------------------------------------------------",
        " intelli-monad MCP server: tools are exposed for supervised client use.",
        " Every tools/call runs in a stateless sandbox bound to session: "
          <> T.encodeUtf8 (scSessionName cfg),
        "-----------------------------------------------------------------------------"
      ]
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout LineBuffering
  inbox <- newTQueueIO :: IO (TQueue (Maybe BC.ByteString))
  -- Reader thread: exits when stdin hits EOF (the transport-close
  -- signal), so a finished server does not spin on hIsEOF forever.
  _ <- forkIO $ readerLoop inbox
  -- Reader: parse each queued line and print responses.
  feed newLineAssembler inbox
  where
    readerLoop inbox = do
      eof <- hIsEOF stdin
      unless eof $ do
        line <- BC.getLine
        -- getLine strips the newline; the assembler splits on newlines,
        -- so re-terminate or the line stays buffered forever.
        atomically (writeTQueue inbox (Just (line <> "\n")))
        readerLoop inbox
      -- EOF: the transport-close signal. Wake the consumer so it can
      -- shut down instead of blocking on the queue forever.
      atomically (writeTQueue inbox Nothing)
    feed asm inbox = do
      ml <- atomically (readTQueue inbox)
      case ml of
        Nothing -> pure () -- EOF: server exits; main returns.
        Just l -> do
          let (events, asm') = feedChunk asm l
          mapM_ respond [l' | LineComplete l' <- events]
          feed asm' inbox
    respond raw =
      case A.eitherDecode' (BL.fromStrict raw) of
        Left err -> logErr ("undecodable line dropped: " <> T.pack err)
        Right v -> do
          out <- handleCall' cfg v
          forM_ out $ \o -> BC.putStrLn (BL.toStrict (A.encode o)) >> hFlush stdout
    logErr t = BC.hPutStrLn stderr ("[mcp-serve] " <> T.encodeUtf8 t)
