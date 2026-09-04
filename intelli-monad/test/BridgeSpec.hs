{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bridge contract tests over the in-memory transport: naming
-- round-trips, tools/list decoding, and tools/call routing.

module BridgeSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Data.Aeson as A
import Data.Aeson ((.=), object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Test.Hspec

import FakeTransport
import IntelliMonad.MCP.Bridge
import IntelliMonad.MCP.Client

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

nextJSONB :: FakeServer -> IO A.Value
nextJSONB fs = do
  mf <- timeout 5000000 (serverNextMessage fs)
  frame <- maybe (fail "timed out waiting for a client frame") pure mf
  either (fail . ("undecodable frame: " <>)) pure (A.eitherDecode' (BL.fromStrict frame))

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "naming" $ do
    it "encodes and parses round-trip" $ do
      parseMcpToolName (mcpToolName "files" "read") `shouldBe` Just ("files", "read")

    it "splits on the last separator (server ids may contain __)" $ do
      parseMcpToolName (mcpToolName "a__b" "c") `shouldBe` Just ("a__b", "c")

    nonMcp :: [Text] <- pure ["", "mcp__", "mcp__onlyserver", "plain_tool", "mcp__x__", "x__y__z"]
    describe "rejects non-MCP names" $ do
      it "rejects the examples" $
        map parseMcpToolName nonMcp `shouldBe` map (const Nothing) nonMcp

  describe "listMcpTools" $ do
    it "decodes a tools/list reply into handles" $
      bracketSession $ \fs s -> do
        done <- newEmptyMVar
        _ <- fork' (putMVar done =<< listMcpTools s "files")
        req <- nextJSONB fs
        KM.lookup "method" (asObj' req) `shouldBe` Just (A.String "tools/list")
        serverReplyResult fs (A.Number 1) $ object
          [ "tools" .=
            [ object ["name" .= ("read" :: Text), "description" .= ("Read a file" :: Text)
                     , "inputSchema" .= object ["type" .= ("object" :: Text)]]
            , object ["name" .= ("write" :: Text)]
            ]
          ]
        r <- takeB' done
        case r of
          Left e -> fail (T.unpack e)
          Right hs -> do
            length hs `shouldBe` 2
            mthName (head hs) `shouldBe` "read"
            mthServer (head hs) `shouldBe` "files"
            mthInputSchema (head hs) `shouldBe` object ["type" .= ("object" :: Text)]
            mthInputSchema (hs !! 1) `shouldBe` A.Object mempty -- absent schema → {}

    it "surfaces a JSON-RPC error as Left" $
      bracketSession $ \fs s -> do
        done <- newEmptyMVar
        _ <- fork' (putMVar done =<< listMcpTools s "files")
        _ <- nextJSONB fs
        serverReplyError fs (A.Number 1) (-32601) "nope"
        r <- takeB' done
        -- listMcpTools surfaces the JSON-RPC error; the server-id
        -- prefix is execMcpTool's job (it knows the owning server).
        r `shouldBe` Left "server error [-32601]: nope"

  describe "registerServer + registeredTools" $ do
    it "registers under encoded names and replaces on re-register" $
      bracketSession $ \fs s -> do
        reg <- newMcpToolRegistry
        done <- newEmptyMVar
        _ <- fork' (putMVar done =<< registerServer reg "files" s)
        _ <- nextJSONB fs
        serverReplyResult fs (A.Number 1) $ object
          [ "tools" .= [object ["name" .= ("read" :: Text)]] ]
        names <- takeB' done
        names `shouldBe` Right ["mcp__files__read"]
        -- Second registration with a different tool set replaces the
        -- server's tools rather than duplicating them.
        done2 <- newEmptyMVar
        _ <- fork' (putMVar done2 =<< registerServer reg "files" s)
        _ <- nextJSONB fs
        serverReplyResult fs (A.Number 2) $ object
          [ "tools" .= [object ["name" .= ("stat" :: Text)]] ]
        _ <- takeB' done2
        tools <- registeredTools reg
        map (\h -> mcpToolName (mthServer h) (mthName h)) tools `shouldBe` ["mcp__files__stat"]

  describe "execMcpTool" $ do
    it "routes a call to the owning server with unwrapped arguments" $
      bracketSession $ \fs s -> do
        reg <- newMcpToolRegistry
        _ <- setupTool reg fs s "files" "read"
        done <- newEmptyMVar
        _ <- fork' (putMVar done =<< execMcpTool reg "mcp__files__read" (asObj' (object ["path" .= ("/tmp/x" :: Text)])))
        req <- nextJSONB fs
        let o = asObj' req
        KM.lookup "method" o `shouldBe` Just (A.String "tools/call")
        params <- case KM.lookup "params" o of
          Just (A.Object p) -> pure p
          _ -> fail "no params"
        KM.lookup "name" params `shouldBe` Just (A.String "read")
        KM.lookup "arguments" params `shouldBe` Just (A.Object (asObj' (object ["path" .= ("/tmp/x" :: Text)])))
        -- The server replies with the MCP tool-result shape.
        serverReplyResult fs (A.Number 2) $ object
          [ "content" .=
            [ object ["type" .= ("text" :: Text), "text" .= ("file contents here" :: Text)] ]
          ]
        r <- takeB' done
        fmap toolResultText r `shouldBe` Right "file contents here"

    it "maps an unknown tool to Left without touching any session" $
      bracketSession $ \_ _ -> do
        reg <- newMcpToolRegistry
        r <- execMcpTool reg "mcp__nowhere__nope" mempty
        r `shouldBe` Left "no such MCP tool: mcp__nowhere__nope"

    it "maps a server JSON-RPC error to Left with server context" $
      bracketSession $ \fs s -> do
        reg <- newMcpToolRegistry
        _ <- setupTool reg fs s "files" "read"
        done <- newEmptyMVar
        _ <- fork' (putMVar done =<< execMcpTool reg "mcp__files__read" mempty)
        _ <- nextJSONB fs
        serverReplyError fs (A.Number 2) (-32000) "disk on fire"
        r <- takeB' done
        r `shouldBe` Left "files: server error [-32000]: disk on fire"

--------------------------------------------------------------------------------
-- Shared setup
--------------------------------------------------------------------------------

-- | Register one tool ("name" = toolName) on a fresh registry.
setupTool :: McpToolRegistry -> FakeServer -> Session -> Text -> Text -> IO ()
setupTool reg fs s server toolName = do
  done <- newEmptyMVar
  _ <- fork' (putMVar done =<< registerServer reg server s)
  _ <- nextJSONB fs
  serverReplyResult fs (A.Number 1) $ object
    [ "tools" .= [object ["name" .= toolName]] ]
  _ <- takeB' done
  pure ()

bracketSession :: (FakeServer -> Session -> IO a) -> IO a
bracketSession k = do
  fs <- newFakePair
  s <- newSession (fsClient fs)
  r <- k fs s
  closeSession s
  pure r

fork' :: IO () -> IO ()
fork' a = do
  _ <- forkIO a
  pure ()

asObj' :: A.Value -> A.Object
asObj' (A.Object o) = o
asObj' other = error ("expected object, got: " <> show other)

takeB' :: MVar a -> IO a
takeB' mv = do
  r <- timeout 5000000 (takeMVar mv)
  maybe (fail "timed out") pure r
