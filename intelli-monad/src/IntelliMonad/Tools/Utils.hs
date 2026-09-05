{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module IntelliMonad.Tools.Utils where

import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.Aeson (encode)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time
import IntelliMonad.MCP.Bridge (execMcpTool, parseMcpToolName, toolResultText)
import IntelliMonad.MCP.Global (globalMcpRegistry)
import IntelliMonad.Types

-- | Execute one tool dispatch step at a concrete type @t@; 'Nothing'
-- when the name doesn't match or the arguments fail to decode.
toolExec' ::
  forall t p m.
  (PersistentBackend p, MonadIO m, MonadFail m, Tool t, A.FromJSON t, A.ToJSON (Output t)) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Prompt m (Maybe Content)
toolExec' sessionName id' name' args' = do
  if name' == toolFunctionName @t
    then case (A.eitherDecode (BS.fromStrict (T.encodeUtf8 args')) :: Either String t) of
      Left _ -> return Nothing
      Right input -> do
        output <- toolExec @t @p @m input
        time <- liftIO getCurrentTime
        return $ Just $ (Content Tool (ToolReturn id' name' (T.decodeUtf8Lenient (BS.toStrict (encode output)))) sessionName time)
    else return Nothing

-- | Choice combinator for dispatch steps: the first 'Just' wins.
(<||>) ::
  forall m.
  (MonadIO m, MonadFail m) =>
  (Text -> Text -> Text -> Text -> Prompt m (Maybe Content)) ->
  (Text -> Text -> Text -> Text -> Prompt m (Maybe Content)) ->
  Text ->
  Text ->
  Text ->
  Text ->
  Prompt m (Maybe Content)
(<||>) tool0 tool1 sessionName id' name' args' = do
  a <- tool0 sessionName id' name' args'
  case a of
    Just v -> return (Just v)
    Nothing -> tool1 sessionName id' name' args'

-- | Dispatch one tool invocation across the toolchain, falling back
-- to the MCP bridge when no static tool claims the name.
mergeToolCall :: forall p m. (PersistentBackend p, MonadIO m, MonadFail m) => [ToolProxy] -> Text -> Text -> Text -> Text -> Prompt m (Maybe Content)
mergeToolCall [] sessionName id' name' args' = mcpFallbackTool sessionName id' name' args'
mergeToolCall (tool : tools') sessionName id' name' args' = do
  case tool of
    (ToolProxy (_ :: Proxy a)) -> (toolExec' @a @p <||> mergeToolCall @p tools') sessionName id' name' args'

-- | Last link in the tool chain: when no compile-time ToolProxy
-- matched, route an @mcp__<server>__<tool>@ call to its MCP server
-- over the global registry. Non-MCP names propagate Nothing unchanged
-- (the historical behavior for unknown tools).
mcpFallbackTool ::
  (MonadIO m, MonadFail m) =>
  Text -> Text -> Text -> Text -> Prompt m (Maybe Content)
mcpFallbackTool sessionName id' name' args'
  | Just _ <- parseMcpToolName name' = do
      let argObj = case A.eitherDecode (BS.fromStrict (T.encodeUtf8 args')) of
            Right (A.Object o) -> o
            _ -> mempty
      result <- liftIO $ execMcpTool globalMcpRegistry name' argObj
      time <- liftIO getCurrentTime
      return $ Just $ case result of
        Right v ->
          Content Tool (ToolReturn id' name' (toolResultText v)) sessionName time
        Left err ->
          Content Tool (ToolReturn id' name' ("MCP error: " <> err)) sessionName time
  | otherwise = return Nothing

-- | Does any content carry a tool invocation?
hasToolCall :: Contents -> Bool
hasToolCall cs =
  let loop [] = False
      loop ((Content _ (ToolCall _ _ _) _ _) : _) = True
      loop (_ : cs') = loop cs'
   in loop cs

-- | Keep only the tool-invocation contents, in order.
filterToolCall :: Contents -> Contents
filterToolCall cs =
  let loop [] = []
      loop (m@(Content _ (ToolCall _ _ _) _ _) : cs') = m : loop cs'
      loop (_ : cs') = loop cs'
   in loop cs

-- | Run every tool invocation in the contents through
-- 'mergeToolCall', returning the tool-return contents.
tryToolExec :: forall p m. (PersistentBackend p, MonadIO m, MonadFail m) => [ToolProxy] -> Text -> Contents -> Prompt m Contents
tryToolExec tools sessionName contents = do
  cs <- forM (filterToolCall contents) $ \(Content _ (ToolCall id' name' args') _ _) -> do
    mergeToolCall @p tools sessionName id' name' args'
  return $ catMaybes cs

-- | Find the first content invoking this tool.
findToolCall :: ToolProxy -> Contents -> Maybe Content
findToolCall _ [] = Nothing
findToolCall t@(ToolProxy (Proxy :: Proxy a)) (c : cs) =
  case c of
    Content _ (Message _) _ _ -> findToolCall t cs
    Content _ (Image _ _) _ _ -> findToolCall t cs
    Content _ (ToolCall _ name' _) _ _ ->
      if name' == toolFunctionName @a
        then Just c
        else findToolCall t cs
    Content _ (ToolReturn _ _ _) _ _ -> findToolCall t cs
