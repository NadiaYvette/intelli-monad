{-# LANGUAGE OverloadedStrings #-}

-- | Expose intelli-monad's compile-time tools as an MCP server over
-- stdio. The inverse of the REPL's @:mcp add@: where the client side
-- lets the REPL *call* other tools, this entry point lets other MCP
-- clients call *ours*.
--
-- Usage:
--
-- > mcp-serve                        # serve all default tools
-- > INTELLIMONAD_MCP_SESSION=foo mcp-serve  # bind tools/call to a session
--
-- The session is selected with the @INTELLIMONAD_MCP_SESSION@
-- environment variable (default @mcp-serve@).
--
-- Protocol: newline-delimited JSON-RPC 2.0 on stdin\/stdout (the stdio
-- transport of the MCP spec). Each @tools\/call@ runs the named tool
-- through 'IntelliMonad.MCP.Server.handleCall' in a fresh stateless
-- environment, so server-side execution cannot leak state across calls.
module Main where

import Data.Text (Text)
import IntelliMonad.MCP.Server
import IntelliMonad.Tools (defaultTools)
import System.Environment (lookupEnv)
import qualified Data.Text as T

main :: IO ()
main = do
  mSession <- lookupEnv "INTELLIMONAD_MCP_SESSION"
  let session :: Text
      session = maybe "mcp-serve" T.pack mSession
      cfg =
        defaultServeConfig
          { scTools = defaultTools,
            scSessionName = session
          }
  serveTools cfg
