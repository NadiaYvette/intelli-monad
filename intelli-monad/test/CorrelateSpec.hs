{-# LANGUAGE OverloadedStrings #-}

module CorrelateSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf, choose)
import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Text as T
import IntelliMonad.MCP.Correlate

-- | Classify-input messages are JSON objects; the constructors carry the
-- KeyMap, so expected values are built through this extractor.
asObj :: A.Value -> A.Object
asObj (A.Object o) = o
asObj other = error ("expected object, got: " <> show other)

spec :: Spec
spec = do
  describe "register" $ do
    it "allocates monotonically increasing ids starting at 1" $ do
      let (_, h1, c1) = register newCorrelator "tools/list"
          (_, h2, c2) = register c1 "tools/call"
      handleId h1 `shouldBe` 1
      handleId h2 `shouldBe` 2
      pendingIds c2 `shouldBe` [1, 2]

  describe "classify" $ do
    it "classifies a success response by id key even when result is null" $ do
      let msg = object ["jsonrpc" .= ("2.0" :: T.Text), "id" .= (7 :: Int), "result" .= A.Null]
      classify msg `shouldBe` InboundResponse (A.toJSON (7 :: Int)) (asObj msg)

    it "classifies an error response" $ do
      let err = object [("code" .= (-32601 :: Int)), ("message" .= ("nope" :: T.Text))]
          msg = object ["jsonrpc" .= ("2.0" :: T.Text), "id" .= (3 :: Int), "error" .= err]
      classify msg `shouldBe` InboundResponse (A.toJSON (3 :: Int)) (asObj msg)

    it "classifies a peer request (method + id)" $ do
      let msg = object ["jsonrpc" .= ("2.0" :: T.Text), "id" .= (9 :: Int), "method" .= ("sampling/createMessage" :: T.Text), "params" .= object []]
      classify msg `shouldBe` InboundRequest "sampling/createMessage" (asObj msg)

    it "classifies a server-initiated request with a string id as a request" $ do
      -- Server->client request ids may legally be strings (JSON-RPC);
      -- they can never collide with our numeric slots.
      let msg = object ["jsonrpc" .= ("2.0" :: T.Text), "id" .= ("srv-1" :: T.Text), "method" .= ("roots/list" :: T.Text)]
      classify msg `shouldBe` InboundRequest "roots/list" (asObj msg)

    it "classifies a notification (method, no id)" $ do
      let msg = object ["jsonrpc" .= ("2.0" :: T.Text), "method" .= ("notifications/initialized" :: T.Text)]
      case classify msg of
        InboundNotification m _ -> m `shouldBe` "notifications/initialized"
        other -> expectationFailure ("wrong classification: " <> show other)

    it "treats a response-shaped message with a string id as a response (never matching our numeric slots)" $ do
      -- Legal for server-initiated traffic; here it simply cannot match
      -- one of our allocated numeric ids and reports NotMine at match
      -- time rather than being dropped as malformed.
      let msg = object ["jsonrpc" .= ("2.0" :: T.Text), "id" .= ("abc" :: T.Text), "result" .= A.Null]
      classify msg `shouldBe` InboundResponse (A.toJSON ("abc" :: T.Text)) (asObj msg)
      takeMatching newCorrelator (classify msg) `shouldBe` NotMine newCorrelator

    it "marks a non-object as malformed" $ do
      classify (A.Number 1) `shouldBe` Malformed "not an object"

    it "marks method+id+result (protocol violation) as malformed, not a response" $ do
      let msg = object ["id" .= (1 :: Int), "method" .= ("x" :: T.Text), "result" .= A.Null]
      case classify msg of
        Malformed _ -> pure ()
        other -> expectationFailure ("wrong classification: " <> show other)

  describe "takeMatching" $ do
    it "matches an open id and removes the slot" $ do
      let (_, h, c1) = register newCorrelator "ping"
          msg = object ["id" .= (handleId h), "result" .= A.Null]
      case takeMatching c1 (classify msg) of
        Matched o c2 ->
          do o `shouldBe` asObj msg
             numPending c2 `shouldBe` 0
        other -> expectationFailure ("no match: " <> show other)

    it "reports a response with a non-numeric id as NotMine" $ do
      let msg = object ["id" .= ("stray" :: T.Text), "result" .= A.Null]
      takeMatching newCorrelator (classify msg) `shouldBe` NotMine newCorrelator

    it "reports a response to an unknown id as NotMine, keeping state" $ do
      let msg = object ["id" .= (99 :: Int), "result" .= A.Null]
      takeMatching newCorrelator (classify msg) `shouldBe` NotMine newCorrelator

    it "survives matching after many interleaved registrations" $ do
      let (_, h1, c1) = register newCorrelator "a"
          (_, _h2, c2) = register c1 "b"
          (_, _h3, c3) = register c2 "c"
          msg = object ["id" .= (handleId h1), "result" .= A.Null]
      case takeMatching c3 (classify msg) of
        Matched _ c4 -> pendingIds c4 `shouldBe` [2, 3]
        other -> expectationFailure ("no match: " <> show other)

  describe "properties" $ do
    prop "every registered id is matchable exactly once" $ \methodIdxs -> do
      let methodNames = ["tools/list", "tools/call", "ping", "resources/read"]
          ms = [ methodNames !! (i `mod` length methodNames)
               | i <- take 8 (methodIdxs ++ cycle [0]) ]
          (hs, cAll) = foldl step ([], newCorrelator) ms
          step (acc, c) m =
            let (_, h, c') = register c m
            in (acc ++ [h], c')
          responses =
            [ object ["id" .= (handleId h), "result" .= A.Null] | h <- hs ]
          matchAll c [] = (0 :: Int, c)
          matchAll c (r:rs) =
            case takeMatching c (classify r) of
              Matched _ c' -> let (n, c'') = matchAll c' rs in (n + 1, c'')
              _ -> matchAll c rs
          (matched, cEnd) = matchAll cAll responses
      matched `shouldBe` length ms
      numPending cEnd `shouldBe` 0
