{-# LANGUAGE OverloadedStrings #-}

module FramingSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Control.Monad (forM_)
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, listOf, listOf1, oneof, property, sized)
import qualified Data.ByteString as BS
import qualified Data.Aeson as A
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Scientific as S

import IntelliMonad.MCP.Framing

spec :: Spec
spec = do
  describe "encodeMessage" $ do
    it "appends exactly one trailing newline" $ do
      encodeMessage (Number 1) `shouldBe` "1\n"

    it "never embeds a raw newline in the frame" $
      forM_ fixedValues $ \v -> do
        let enc = encodeMessage (v :: Value)
        BS.length (BS.filter (== 10) enc) `shouldBe` 1
        BS.last enc `shouldBe` 10

  describe "decodeBuffer" $ do
    it "decodes a single line" $ do
      let (vs, es, t) = decodeBuffer "{\"a\":1}\n"
      vs `shouldBe` [object [("a" .= Number 1)]]
      es `shouldBe` []
      t `shouldBe` Nothing

    it "skips empty lines" $ do
      let (vs, es, t) = decodeBuffer "\n\n{\"a\":1}\n\n"
      vs `shouldBe` [object [("a" .= Number 1)]]
      es `shouldBe` []
      t `shouldBe` Nothing

    it "returns trailing bytes for an incomplete final message" $ do
      let (vs, es, t) = decodeBuffer "{\"a\":1}\n{\"b"
      vs `shouldBe` [object [("a" .= Number 1)]]
      es `shouldBe` []
      t `shouldBe` Just "{\"b"

    it "isolates malformed lines as DecodeError without losing siblings" $ do
      let (vs, es, t) = decodeBuffer "{\"a\":1}\nnot json\n[2]\n"
      length vs `shouldBe` 2
      length es `shouldBe` 1
      dePayload (head es) `shouldBe` "not json"
      t `shouldBe` Nothing

    it "handles an empty buffer" $ do
      decodeBuffer BS.empty `shouldBe` ([], [], Nothing)

  describe "feedChunk" $ do
    it "buffers an incomplete line across chunks" $ do
      let (ev1, a1) = feedChunk newLineAssembler "{\"a\":"
          (ev2, a2) = feedChunk a1 "1}\n"
      ev1 `shouldBe` []
      assemblerPending a1 `shouldBe` "{\"a\":"
      ev2 `shouldBe` [LineComplete "{\"a\":1}"]
      assemblerPending a2 `shouldBe` BS.empty

    it "consumes empty lines silently" $ do
      let (evs, a) = feedChunk newLineAssembler "\n\n\n"
      evs `shouldBe` []
      assemblerPending a `shouldBe` BS.empty

    prop "round-trips values through encode + irregular chunked feed" $ \jvs -> do
      let vals = map jvValue (jvs :: [JVal])
          payload = BS.concat (map encodeMessage vals)
          (events, final) = feedChunks newLineAssembler (chop payload)
          got = [l | LineComplete l <- events]
      -- LineComplete carries the line WITHOUT its framing newline;
      -- re-adding newlines must reproduce the exact byte stream.
      BS.concat (map (<> "\n") got) `shouldBe` payload
      BS.null (assemblerPending final) `shouldBe` True

-- Helpers ------------------------------------------------------------

feedChunks :: LineAssembler -> [BS.ByteString] -> ([AssemblerEvent], LineAssembler)
feedChunks a [] = ([], a)
feedChunks a (c:cs) =
  let (ev1, a1) = feedChunk a c
      (ev2, a2) = feedChunks a1 cs
  in (ev1 ++ ev2, a2)

-- Chop into irregular 1-5 byte chunks to stress boundary handling.
chop :: BS.ByteString -> [BS.ByteString]
chop bs
  | BS.null bs = []
  | otherwise =
      let n = 1 + (BS.length bs `mod` 5)
      in BS.take n bs : chop (BS.drop n bs)

-- | Fixed representative values for the newline-free invariant.
fixedValues :: [Value]
fixedValues =
  [ Null
  , Bool True
  , Number 42
  , String "2025-06-18"
  , object ["method" .= ("tools/list" :: String), "id" .= (1 :: Int)]
  , object ["params" .= object ["nested" .= ("{\"deep\":true}" :: String)]]
  ]

-- | A bounded JSON generator: flat objects and scalars, the shape of
-- well-formed MCP stdio traffic.
newtype JVal = JVal { jvValue :: Value }

instance Show JVal where
  show = show . jvValue

instance Eq JVal where
  a == b = jvValue a == jvValue b

instance Arbitrary JVal where
  arbitrary = oneof
    [ pure (JVal Null)
    , JVal . Bool <$> arbitrary
    , JVal . Number . fromInteger <$> choose (0, 10000 :: Integer)
    , JVal . String <$> elements ["x", "hello world", "2025-06-18"]
    , JVal . object <$> listOf ((.=) <$> elements ["a", "method", "id"] <*> genScalar)
    ]
    where
      genScalar = oneof
        [ pure Null
        , Bool <$> arbitrary
        , Number . fromInteger <$> choose (0, 1000 :: Integer)
        , String <$> elements ["x", "tools/list"]
        ]
