{-# LANGUAGE OverloadedStrings #-}

-- | Client session contract tests over the in-memory transport: the
-- observable wire behavior of 'newSession' / 'initialize' /
-- 'mcpRequest' / 'mcpNotify' / 'respondToServer'.

module ClientSpec (spec) where

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
import IntelliMonad.MCP.Client
import IntelliMonad.MCP.Framing (encodeMessage)
import IntelliMonad.MCP.Negotiate

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

asObj :: A.Value -> A.Object
asObj (A.Object o) = o
asObj other = error ("expected object, got: " <> show other)

-- | Run an action with a session over a fresh fake pair, closing the
-- session afterwards.
withFake :: (FakeServer -> Session -> IO a) -> IO a
withFake k = do
  fs <- newFakePair
  s <- newSession (fsClient fs)
  r <- k fs s
  closeSession s
  pure r

-- | Take the next frame the client sent and decode it — tests assert
-- on JSON shape, not bytes. Bounded so a regression fails the test
-- instead of hanging the suite.
nextJSON :: FakeServer -> IO A.Value
nextJSON fs = do
  mf <- timeout 5000000 (serverNextMessage fs)
  frame <- maybe (fail "timed out waiting for a client frame") pure mf
  either (fail . ("undecodable frame: " <>)) pure (A.eitherDecode' (BL.fromStrict frame))

-- | A bounded take so a regression fails the test instead of hanging
-- the suite (the whole suite must stay fast — Phase 1's lesson).
takeBounded :: MVar a -> IO a
takeBounded mv = do
  r <- timeout 5000000 (takeMVar mv)
  maybe (fail "timed out waiting for completion") pure r

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "mcpRequest" $ do
    it "sends a correlated request and completes on its response" $
      withFake $ \fs s -> do
        done <- newEmptyMVar
        _ <- forkIO (putMVar done =<< mcpRequest s "tools/list" Nothing 5000)
        req <- nextJSON fs
        let o = asObj req
        KM.lookup "id" o `shouldBe` Just (A.Number 1)
        KM.lookup "method" o `shouldBe` Just (A.String "tools/list")
        KM.member "params" o `shouldBe` False
        serverReplyResult fs (A.Number 1) (object ["tools" .= (1 :: Int)])
        got <- takeBounded done
        got `shouldBe` Right (object ["tools" .= (1 :: Int)])

    it "completes out-of-order responses correctly" $
      withFake $ \fs s -> do
        d1 <- newEmptyMVar
        d2 <- newEmptyMVar
        _ <- forkIO (putMVar d1 =<< mcpRequest s "a" Nothing 5000)
        _ <- forkIO (putMVar d2 =<< mcpRequest s "b" Nothing 5000)
        r1 <- nextJSON fs
        r2 <- nextJSON fs
        KM.lookup "id" (asObj r1) `shouldBe` Just (A.Number 1)
        KM.lookup "id" (asObj r2) `shouldBe` Just (A.Number 2)
        -- Reply to the second request first.
        serverReplyResult fs (A.Number 2) (A.String "second")
        serverReplyResult fs (A.Number 1) (A.String "first")
        got1 <- takeBounded d1
        got2 <- takeBounded d2
        got1 `shouldBe` Right (A.String "first")
        got2 `shouldBe` Right (A.String "second")

    it "times out a request that gets no response" $
      withFake $ \_ s -> do
        r <- mcpRequest s "slow" Nothing 100
        case r of
          Left why -> T.unpack why `shouldContain` "timed out"
          Right v -> expectationFailure ("unexpected success: " <> show v)

    it "surfaces a JSON-RPC error response as Left" $
      withFake $ \fs s -> do
        done <- newEmptyMVar
        _ <- forkIO (putMVar done =<< mcpRequest s "nope" Nothing 5000)
        _ <- nextJSON fs
        serverReplyError fs (A.Number 1) (-32601) "no such method"
        r <- takeBounded done
        case r of
          Left why -> T.unpack why `shouldContain` "no such method"
          Right v -> expectationFailure (show v)

    it "stays usable after a timeout (late response is dropped)" $
      withFake $ \fs s -> do
        _ <- mcpRequest s "late" Nothing 100
        _ <- nextJSON fs -- drain the "late" request frame
        -- The late response for id 1 arrives after the waiter left.
        serverReplyResult fs (A.Number 1) (A.String "late")
        threadDelay 150000
        -- The session still works and allocates fresh ids.
        done <- newEmptyMVar
        _ <- forkIO (putMVar done =<< mcpRequest s "ping" Nothing 5000)
        req <- nextJSON fs
        KM.lookup "id" (asObj req) `shouldBe` Just (A.Number 2)
        serverReplyResult fs (A.Number 2) (A.String "pong")
        got <- takeBounded done
        got `shouldBe` Right (A.String "pong")

  describe "mcpNotify" $ do
    it "sends notifications without id and without waiting" $
      withFake $ \fs s -> do
        r <- mcpNotify s "notifications/initialized" Nothing
        r `shouldBe` Right ()
        n <- nextJSON fs
        let o = asObj n
        KM.lookup "method" o `shouldBe` Just (A.String "notifications/initialized")
        KM.member "id" o `shouldBe` False

  describe "initialize" $ do
    it "performs the handshake and caches the negotiation" $
      withFake $ \fs s -> do
        done <- newEmptyMVar
        _ <- forkIO (putMVar done =<< initialize s)
        req <- nextJSON fs
        let o = asObj req
        KM.lookup "method" o `shouldBe` Just (A.String "initialize")
        params <- case KM.lookup "params" o of
          Just (A.Object p) -> pure p
          _ -> fail "initialize request has no params object"
        KM.lookup "protocolVersion" params `shouldBe` Just (A.String "2025-06-18")
        -- Server agrees on our proposal.
        serverReplyResult fs (A.Number 1) (object ["protocolVersion" .= ("2025-06-18" :: Text)])
        en <- takeBounded done
        neg <- case en of
          Left e -> fail ("initialize failed: " <> T.unpack e)
          Right n -> pure n
        nAgreed neg `shouldBe` R2025_06_18
        -- The initialized notification follows the response.
        note <- nextJSON fs
        KM.lookup "method" (asObj note) `shouldBe` Just (A.String "notifications/initialized")
        -- And the outcome is cached.
        neg2 <- negotiated s
        fmap nAgreed neg2 `shouldBe` Just R2025_06_18

    it "refuses a second initialize" $
      withFake $ \fs s -> do
        done <- newEmptyMVar
        _ <- forkIO (putMVar done =<< initialize s)
        _ <- nextJSON fs
        serverReplyResult fs (A.Number 1) (object ["protocolVersion" .= ("2025-06-18" :: Text)])
        _ <- takeBounded done :: IO (Either Text Negotiation)
        r2 <- initialize s
        case r2 of
          Left _ -> pure ()
          Right _ -> expectationFailure "second initialize should fail"

  describe "respondToServer" $ do
    it "answers server-initiated requests with the installed handler" $
      withFake $ \fs s -> do
        let handler _m _o = pure (object ["roots" .= (2 :: Int)])
        respondToServer s (Just handler)
        -- The "server" sends roots/list with a string id (legal).
        serverSendValue fs (object ["jsonrpc" .= ("2.0" :: Text), "id" .= ("srv-1" :: Text), "method" .= ("roots/list" :: Text)])
        reply <- nextJSON fs
        let o = asObj reply
        KM.lookup "id" o `shouldBe` Just (A.String "srv-1")
        KM.lookup "result" o `shouldBe` Just (object ["roots" .= (2 :: Int)])

    it "refuses server-initiated requests with method-not-found when no handler is installed" $
      withFake $ \fs s -> do
        serverSendValue fs (object ["jsonrpc" .= ("2.0" :: Text), "id" .= (7 :: Int), "method" .= ("sampling/createMessage" :: Text)])
        reply <- nextJSON fs
        let o = asObj reply
        KM.lookup "id" o `shouldBe` Just (A.Number 7)
        case KM.lookup "error" o of
          Just (A.Object e) -> KM.lookup "code" e `shouldBe` Just (A.Number (-32601))
          other -> expectationFailure ("expected error object: " <> show other)

  describe "closeSession" $ do
    it "makes subsequent requests fail fast, and is idempotent" $
      withFake $ \fs s -> do
        closeSession s
        closeSession s -- idempotent
        r <- mcpRequest s "ping" Nothing 1000
        r `shouldBe` Left "session is closed"
