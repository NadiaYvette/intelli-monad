{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Process-wide MCP state: the 'McpToolRegistry' singleton and the
-- server-id → launch-config table behind the REPL's @:mcp@ command.
--
-- GHC's standard pattern for a top-level mutable value:
-- 'unsafePerformIO' of the initializer, 'NOINLINE' so the CAF is
-- shared instead of re-run at every call site. Both "sides" of the
-- REPL must see the same registry: @:mcp add@ writes it, the tool
-- chain reads it, and request assembly reads it — all through this
-- module. The MCP modules themselves stay pure-with-respect-to-globals
-- (a 'McpToolRegistry' is created explicitly and passed); only this
-- glue owns the global instance.
--
-- Lifecycle note: sessions launched here are closed only by
-- @:mcp remove@. The OS reaps children on process exit, so a REPL
-- quitting without @:mcp remove@ is safe, just less graceful.

module IntelliMonad.MCP.Global
  ( -- * Global registry
    globalMcpRegistry
  , mcpToolProxiesHint
    -- * Server management (REPL-facing)
  , mcpAddServer
  , mcpRemoveServer
  , mcpListServers
  , mcpServerToolNames
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

import IntelliMonad.MCP.Bridge
import IntelliMonad.MCP.Client (Session, closeSession, newSession)
import IntelliMonad.MCP.Transport

-- | The process-wide registry. See module docs for why this exists.
{-# NOINLINE globalMcpRegistry #-}
globalMcpRegistry :: McpToolRegistry
globalMcpRegistry = unsafePerformIO newMcpToolRegistry

-- | Launch configuration for one MCP server (the @:mcp add@ payload).
data ServerSpec = ServerSpec
  { ssProgram :: FilePath
  , ssArgs :: [String]
  }

-- | Server id → how to launch it. Kept so @:mcp remove@ can close the
-- session it started, and so a future reconnect has the spec.
{-# NOINLINE serverSpecs #-}
serverSpecs :: MVar (M.Map McpServerId (ServerSpec, TransportHandle))
serverSpecs = unsafePerformIO (newMVar M.empty)

-- | Opaque record of what 'mcpAddServer' created and must later close.
data TransportHandle = TransportHandle
  { thSession :: Session
  , thClose :: IO ()
  }

-- | Connect to an MCP server (spawning it if a program is given) and
-- register its tools under @server@.
--
-- The program is resolved via @PATH@; args are passed verbatim (no
-- shell). Re-adding an id removes the previous session first.
mcpAddServer :: McpServerId -> FilePath -> [String] -> IO (Either Text [Text])
mcpAddServer server prog args = do
  -- Replace semantics: drop any previous connection for this id.
  _ <- mcpRemoveServer server
  opened <- try (openStdIO (defaultStdIOConfig prog) { sioArgs = args })
  case opened of
    Left (e :: SomeException) -> pure (Left (T.pack (show e)))
    Right trans -> do
      s <- newSession trans
      r <- registerServer globalMcpRegistry server s
      case r of
        Left e -> do
          tClose trans
          pure (Left e)
        Right names -> do
          modifyMVar_ serverSpecs $
            pure . M.insert server (ServerSpec prog args, TransportHandle s (tClose trans))
          pure (Right names)

-- | Close a server's session and forget its tools.
mcpRemoveServer :: McpServerId -> IO Bool
mcpRemoveServer server = do
  mh <- modifyMVar serverSpecs (pure . \m -> (M.delete server m, M.lookup server m))
  case mh of
    Nothing -> pure False
    Just (_, h) -> do
      closeSession (thSession h)
      pure True

-- | Connected server ids with their discovered tool counts.
mcpListServers :: IO [(McpServerId, Int)]
mcpListServers = do
  tools <- registeredTools globalMcpRegistry
  specs <- readMVar serverSpecs
  pure [(sid, length [() | t <- tools, mthServer t == sid]) | sid <- M.keys specs]

-- | Encoded tool names for one server (for @:mcp list@ display).
mcpServerToolNames :: McpServerId -> IO [Text]
mcpServerToolNames server = do
  tools <- registeredTools globalMcpRegistry
  pure [mcpToolName server (mthName t) | t <- tools, mthServer t == server]

-- | Display hint for users: MCP tools need no ToolProxy — they join
-- the chain through 'mcpFallbackTool'. Exported so :help can print it.
mcpToolProxiesHint :: Text
mcpToolProxiesHint = "MCP tools are discovered at runtime; use :mcp list to see them."
