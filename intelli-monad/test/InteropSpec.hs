{-# LANGUAGE OverloadedStrings #-}

-- | C4 acceptance: the PMWA interop criterion. The fixture pair's
-- claim verifies on its own evidence; off-domain and effectful
-- shapes refuse or stay unproven, exactly as the milestone defines.
module InteropSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import IntelliMonad.Tools.OrganBank.Dictionary
  ( Family (..)
  , Member (..)
  )
import IntelliMonad.Tools.OrganBank.Interop
  ( InteropOutcome (..)
  , InteropPair (..)
  , pmwaWitnessDomain
  , runInterop
  )
import IntelliMonad.Tools.OrganBank.Stubs
  ( Position (..)
  , StubRequest (..)
  , fixtureHaskellRust
  )

m :: Family -> Maybe Int -> Text -> Member
m f w note = Member f w note

spec :: Spec
spec = do
  describe "runInterop: the fixture pair (haskell Int# <-> rust i64)" $ do
    it "verifies on its own evidence" $
      ioVerdict (runInterop fixtureHaskellRust) `shouldBe` "pmwa-verified"
    it "identifies the pair by its endpoint languages" $ do
      let o = runInterop fixtureHaskellRust
      (ipCallerLang (ioPair o), ipCalleeLang (ioPair o)) `shouldBe` ("haskell", "rust")
    it "shows all three legs" $ do
      let o = runInterop fixtureHaskellRust
      ioRepresentation o `shouldSatisfy` maybe False ("verified: " `T.isPrefixOf`)
      ioConvention o `shouldSatisfy` maybe False ("verified: " `T.isPrefixOf`)
      ioEffects o `shouldSatisfy` maybe False ("verified: " `T.isPrefixOf`)
    it "records the witness domain" $
      ioWitnessDomain (runInterop fixtureHaskellRust) `shouldBe` pmwaWitnessDomain

  describe "runInterop: leg 1, representation" $ do
    it "refuses when a position leaves the scalar wire domain" $ do
      let req =
            fixtureHaskellRust
              { srPositions =
                  [ Position "arg 0" (m FBigSigned Nothing "GHC Integer") (m FSigned (Just 64) "std/i64"),
                    Position "result" (m FSigned (Just 64) "std/i64") (m FSigned (Just 64) "ghc-prim/Int#")
                  ]
              }
      ioVerdict (runInterop req) `shouldBe` "pmwa-refused-representation"
      ioRepresentation (runInterop req) `shouldBe` Nothing
    it "still reports the legs it did reach" $ do
      let req =
            fixtureHaskellRust
              { srPositions =
                  [ Position "arg 0" (m FDynamic Nothing "json") (m FSigned (Just 64) "std/i64"),
                    Position "result" (m FSigned (Just 64) "std/i64") (m FSigned (Just 64) "ghc-prim/Int#")
                  ]
              }
          o = runInterop req
      ioVerdict o `shouldBe` "pmwa-refused-representation"
      ioConvention o `shouldSatisfy` maybe False ("verified: " `T.isPrefixOf`)

  describe "runInterop: leg 3, effects" $ do
    it "delegates a dictionary refusal to a pmwa-refused verdict" $ do
      -- Caller demands a power the callee lacks: refused by the subset
      -- rule before any glue exists.
      let req =
            fixtureHaskellRust
              { srCallerEffects = ["std/core/exn"],
                srCalleeEffects = []
              }
      ioVerdict (runInterop req) `shouldBe` "pmwa-refused-effects"
      ioEffects (runInterop req) `shouldSatisfy` maybe False ("refused: " `T.isPrefixOf`)
    it "stays unproven when the map is requested but not generatable" $ do
      -- haskell callee: no generatable island-side mapping (fail-closed).
      let req =
            fixtureHaskellRust
              { srCalleeEffects = ["std/core/exn"],
                srEffectMap = True
              }
      ioVerdict (runInterop req) `shouldBe` "pmwa-unproven-effects"
      ioEffects (runInterop req) `shouldSatisfy` maybe False ("unproven: " `T.isPrefixOf`)
    it "verifies a pure crossing with no map requested" $ do
      ioEffects (runInterop fixtureHaskellRust)
        `shouldBe` Just "verified: effect rows empty on both sides (pure crossing)"
