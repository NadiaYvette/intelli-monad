{-# LANGUAGE OverloadedStrings #-}

module NegotiateSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (elements, forAll)
import qualified Data.Text as T

import IntelliMonad.MCP.Negotiate

spec :: Spec
spec = do
  describe "parseRevision" $ do
    it "parses all five revision strings" $
      map (fmap revisionString . parseRevision)
        ["2024-11-05", "2025-03-26", "2025-06-18", "2025-11-25", "2026-07-28"]
        `shouldBe` map (Just . revisionString) allRevisions

    it "rejects unknown and malformed versions" $ do
      parseRevision "1999-01-01" `shouldBe` Nothing
      parseRevision "" `shouldBe` Nothing
      parseRevision "latest" `shouldBe` Nothing

  describe "supportsFeature (the matrix as executable rows)" $ do
    it "batching exists only in 2025-03-26" $
      [supportsFeature r FBatching | r <- allRevisions]
        `shouldBe` [False, True, False, False, False]

    it "streamable http from 2025-03-26 onward" $
      [supportsFeature r FStreamableHttp | r <- allRevisions]
        `shouldBe` [False, True, True, True, True]

    it "http+sse only in the first revision" $
      [supportsFeature r FHttpSse | r <- allRevisions]
        `shouldBe` [True, False, False, False, False]

    it "structured tool output from the target revision onward" $
      [supportsFeature r FStructuredToolOutput | r <- allRevisions]
        `shouldBe` [False, False, True, True, True]

    it "elicitation introduced at target, reworked one revision later" $
      [ (supportsFeature r FElicitation, supportsFeature r FElicitationReworked)
      | r <- allRevisions
      ]
        `shouldBe` [ (False, False), (False, False), (True, False), (True, True), (True, True) ]

    it "stateless and no-initialize only in 2026-07-28" $ do
      [supportsFeature r FStateless | r <- allRevisions]
        `shouldBe` [False, False, False, False, True]
      [supportsFeature r FRequiresInitialize | r <- allRevisions]
        `shouldBe` [True, True, True, True, False]

    it "sampling tools and tasks from 2025-11-25 onward" $
      [ (supportsFeature r FSamplingTools, supportsFeature r FTasks)
      | r <- allRevisions
      ]
        `shouldBe` [ (False, False), (False, False), (False, False), (True, True), (True, True) ]

  describe "negotiateWith" $ do
    let target = clientDefault

    it "agrees exactly when the server speaks our proposal" $ do
      let n = negotiateWith target "2025-06-18"
      nAgreed n `shouldBe` R2025_06_18
      nNote n `shouldSatisfy` T.isInfixOf "agreed"

    it "downgrades cleanly to an older server" $ do
      let n = negotiateWith target "2024-11-05"
      nAgreed n `shouldBe` R2024_11_05
      nNote n `shouldSatisfy` T.isInfixOf "downgraded"

    it "falls back to our proposal on an unknown server version" $ do
      let n = negotiateWith target "2027-01-01"
      nAgreed n `shouldBe` target
      nNote n `shouldSatisfy` T.isInfixOf "unknown"

    it "falls back when the server is newer than we can speak" $ do
      let n = negotiateWith target "2026-07-28"
      nAgreed n `shouldBe` target
      nNote n `shouldSatisfy` T.isInfixOf "newer"

    it "never agrees above what we proposed, for any server reply" $
      forAll (elements (map revisionString allRevisions ++ ["2027-01-01", "garbage", ""])) $
      \serverStr ->
        let n = negotiateWith target serverStr
        in nAgreed n <= target

    it "handles every known server revision without crashing" $
      mapM_ (\r -> do
        let n = negotiateWith target (revisionString r)
        nAgreed n `shouldSatisfy` (\a -> a == target || a == r)) allRevisions

  describe "recorded handshake fixtures (one per row)" $ do
    it "row 2024-11-05: downgrade + no streamable http" $ do
      let n = negotiateWith clientDefault "2024-11-05"
      nAgreed n `shouldBe` R2024_11_05
      supportsFeature (nAgreed n) FStreamableHttp `shouldBe` False
      supportsFeature (nAgreed n) FRequiresInitialize `shouldBe` True

    it "row 2025-03-26: downgrade + batching quirks" $ do
      let n = negotiateWith clientDefault "2025-03-26"
      nAgreed n `shouldBe` R2025_03_26
      supportsFeature (nAgreed n) FBatching `shouldBe` True

    it "row 2025-06-18: exact agreement, elicitation available" $ do
      let n = negotiateWith clientDefault "2025-06-18"
      nAgreed n `shouldBe` R2025_06_18
      supportsFeature (nAgreed n) FElicitation `shouldBe` True

    it "row 2025-11-25: server newer than our proposal, we stay put" $ do
      let n = negotiateWith clientDefault "2025-11-25"
      nRequested n `shouldBe` R2025_06_18
      nAgreed n `shouldBe` R2025_06_18
      supportsFeature (nAgreed n) FSamplingTools `shouldBe` False

    it "row 2025-11-25: proposing it directly unlocks sampling tools" $ do
      let n = negotiateWith R2025_11_25 "2025-11-25"
      nAgreed n `shouldBe` R2025_11_25
      supportsFeature (nAgreed n) FSamplingTools `shouldBe` True

    it "row 2026-07-28: newer than target, client stays put" $ do
      let n = negotiateWith clientDefault "2026-07-28"
      nAgreed n `shouldBe` clientDefault
