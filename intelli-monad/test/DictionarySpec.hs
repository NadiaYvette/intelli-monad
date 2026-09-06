{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the representation dictionary
-- ("IntelliMonad.Tools.OrganBank.Dictionary"): the axiom table's
-- sanity, the license rules, fail-closed aggregation, and the
-- direction-aware integration with 'boundaryReport'.
module DictionarySpec (spec) where

import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import IntelliMonad.Tools.OrganBank
  ( OrganCheckBoundary (..),
    BoundaryReport (..),
    boundaryReport,
    typeHeadline,
  )
import IntelliMonad.Tools.OrganBank.Dictionary
  ( Family (..),
    Member (..),
    aggregate,
    license,
    memberOf,
  )

spec :: Spec
spec = do
  describe "memberOf (axiom table)" $ do
    it "resolves entries from the real corpus qnames" $ do
      fmap mFamily (memberOf "c" "std" "int32") `shouldBe` Just FSigned
      fmap mWidth (memberOf "c" "std" "int32") `shouldBe` Just (Just (32 :: Int))
      fmap mFamily (memberOf "haskell" "ghc-prim" "Int#") `shouldBe` Just FSigned
      fmap mWidth (memberOf "haskell" "ghc-prim" "Int#") `shouldBe` Just (Just (64 :: Int))
      fmap mFamily (memberOf "rust" "std" "i64") `shouldBe` Just FSigned
      fmap mFamily (memberOf "zig" "std" "u64") `shouldBe` Just FUnsigned
      fmap mFamily (memberOf "ocaml" "Stdlib" "int") `shouldBe` Just FSigned
      -- The honest entries: implementation-defined precision.
      fmap mWidth (memberOf "sml" "Basis" "int") `shouldBe` Just Nothing
      fmap mWidth (memberOf "mercury" "std" "int") `shouldBe` Just Nothing
      fmap mFamily (memberOf "lean4" "Lean" "Nat") `shouldBe` Just FBigUnsigned
      fmap mFamily (memberOf "koka" "std/core" "integer") `shouldBe` Just FBigSigned

    it "is case-insensitive on the language" $
      fmap mFamily (memberOf "Haskell" "ghc-prim" "Int#") `shouldBe` Just FSigned

    it "recognizes the dynamic marker for every language" $ do
      fmap mFamily (memberOf "lua" "std" "any") `shouldBe` Just FDynamic
      fmap mFamily (memberOf "prolog" "std" "any") `shouldBe` Just FDynamic

    it "returns Nothing outside the table (fail closed)" $
      memberOf "c" "std" "int24" `shouldBe` Nothing

  describe "license" $ do
    let m f w note = Member f w note
    it "licenses equal-width same-family crossings as lossless" $
      license (m FSigned (Just 32) "a") (m FSigned (Just 32) "b") `shouldBe`
        ("licensed-lossless", ["axiom: signed-int (32 bits) — a", "axiom: signed-int (32 bits) — b", "same family, no range question"])

    it "licenses same-family same-width regardless of the note text" $ do
      let (v, _) = license (Member FSigned (Just 64) "x") (Member FSigned (Just 64) "y")
      v `shouldBe` "licensed-lossless"

    it "licenses widening only upward" $ do
      let (vUp, _) = license (m FSigned (Just 32) "") (m FSigned (Just 64) "")
          (vDn, _) = license (m FSigned (Just 64) "") (m FSigned (Just 32) "")
      vUp `shouldBe` "licensed-widening"
      vDn `shouldBe` "unlicensed-narrowing"

    it "refuses signed-into-unsigned even when wider" $ do
      let (v, ax) = license (m FSigned (Just 32) "") (m FUnsigned (Just 64) "")
      v `shouldBe` "unlicensed-overflow-domain"
      any ("admits negatives" `T.isInfixOf`) ax `shouldBe` True

    it "refuses same-width different-signedness" $ do
      let (v1, _) = license (m FSigned (Just 32) "") (m FUnsigned (Just 32) "")
          (v2, _) = license (m FUnsigned (Just 32) "") (m FSigned (Just 32) "")
      v1 `shouldBe` "unlicensed-overflow-domain"
      v2 `shouldBe` "unlicensed-overflow-domain"

    it "refuses different families outright" $ do
      let (v, _) = license (m FSigned (Just 32) "") (m FFloat (Just 32) "")
      v `shouldBe` "unlicensed-family"

    it "refuses implementation-defined precision (no range axiom)" $ do
      let (v, _) = license (Member FSigned Nothing "sml int") (m FSigned (Just 64) "")
      v `shouldBe` "unlicensed-range"

    it "licenses bigint pairs by domain: signed->signed lossless, signed->Nat refused, Nat->signed lossless" $ do
      let (vSS, _) = license (Member FBigSigned Nothing "a") (Member FBigSigned Nothing "b")
          (vSU, axSU) = license (Member FBigSigned Nothing "a") (Member FBigUnsigned Nothing "b")
          (vUS, _) = license (Member FBigUnsigned Nothing "a") (Member FBigSigned Nothing "b")
      vSS `shouldBe` "licensed-lossless"
      vSU `shouldBe` "unlicensed-overflow-domain"
      any ("below zero" `T.isInfixOf`) axSU `shouldBe` True
      vUS `shouldBe` "licensed-lossless"

    it "refuses bigint against fixed-width as a representation mismatch (never a width guess)" $ do
      let (v1, ax1) = license (Member FBigSigned Nothing "arbitrary") (m FSigned (Just 64) "")
          (v2, _) = license (m FSigned (Just 64) "") (Member FBigSigned Nothing "arbitrary")
          (v3, _) = license (Member FBigUnsigned Nothing "arbitrary") (m FUnsigned (Just 32) "")
      v1 `shouldBe` "unlicensed-representation"
      any ("unbounded" `T.isInfixOf`) ax1 `shouldBe` True
      v2 `shouldBe` "unlicensed-representation"
      v3 `shouldBe` "unlicensed-representation"

    it "admits dynamic sides only with runtime checks" $ do
      let (v1, _) = license (m FDynamic Nothing "") (m FSigned (Just 64) "")
          (v2, _) = license (m FSigned (Just 64) "") (m FDynamic Nothing "")
      v1 `shouldBe` "licensed-with-runtime-checks"
      v2 `shouldBe` "licensed-with-runtime-checks"

    it "lossless families ignore width" $ do
      let (v, _) = license (m FBool Nothing "") (m FBool Nothing "")
      v `shouldBe` "licensed-lossless"

  describe "aggregate (weakest link, fail closed)" $ do
    it "is Nothing on an empty position list" $
      aggregate [] `shouldBe` Nothing

    it "keeps the weakest verdict" $ do
      aggregate ["licensed-lossless", "licensed-widening"] `shouldBe` Just "licensed-widening"
      aggregate ["licensed-widening", "licensed-lossless"] `shouldBe` Just "licensed-widening"

    it "ranks runtime checks above widening" $
      aggregate ["licensed-lossless", "licensed-with-runtime-checks"] `shouldBe` Just "licensed-with-runtime-checks"

    it "any unlicensed verdict poisons the call" $
      aggregate ["licensed-lossless", "unlicensed-narrowing", "licensed-widening"]
        `shouldBe` Just "unlicensed-narrowing"

    it "unknown verdicts fail closed" $
      aggregate ["licensed-lossless", "something-new"] `shouldBe` Just "something-new"

  describe "boundaryReport dictionary integration (direction-aware)" $ do
    let args = OrganCheckBoundary "Factorial" "factorial" (Just "c") "factorial_rs" "factorial" (Just "rust")
    it "licenses c int32 -> rust i64 args but refuses the narrowing return" $ do
      let c32 = fnMod "std" "int32" "int32"
          r64 = fnMod "std" "i64" "i64"
          r = boundaryReport args (Just "c") c32 (typeHeadline c32) (Just "rust") r64 (typeHeadline r64)
      brVerdict r `shouldBe` "unlicensed-boundary"
      brDetail r `shouldSatisfy` elem "arg 1: licensed-widening"
      brDetail r `shouldSatisfy` elem "result: unlicensed-narrowing"

    it "licenses the fully-widened direction (c int32 -> rust i64 with i32 return)" $ do
      -- Return direction is callee -> caller: rust i32 into c int32 is
      -- same-width lossless, so the whole call is widening on args.
      let c32 = fnMod "std" "int32" "int32"
          r = fnMod "std" "i64" "i32"
          rep = boundaryReport args (Just "c") c32 (typeHeadline c32) (Just "rust") r (typeHeadline r)
      brVerdict rep `shouldBe` "licensed-widening"
      brDetail rep `shouldContain` ["Representation dictionary: the callee's representation contains the caller's range (widening); a stub must convert explicitly."]

    it "licenses lossless when both sides carry identical axiom families and widths" $ do
      let h = fnMod "ghc-prim" "Int#" "Int#"
          k = fnMod "std/core" "int" "int"
          rep = boundaryReport args (Just "haskell") h (typeHeadline h) (Just "koka") k (typeHeadline k)
      brVerdict rep `shouldBe` "licensed-lossless"

    it "fails closed when a qname has no axiom" $ do
      let h = fnMod "ghc-prim" "Int#" "Int#"
          p = fnMod "std" "int" "int" -- prolog has no int axiom and this is not the dynamic marker
          rep = boundaryReport args (Just "haskell") h (typeHeadline h) (Just "prolog") p (typeHeadline p)
      brVerdict rep `shouldBe` "unlicensed-boundary"
      any ("outside the dictionary" `T.isInfixOf`) (brDetail rep) `shouldBe` True

    it "admits dynamic sides only with runtime checks" $ do
      let h = fnMod "ghc-prim" "Int#" "Int#"
          p = fnMod "std" "any" "any"
          rep = boundaryReport args (Just "haskell") h (typeHeadline h) (Just "prolog") p (typeHeadline p)
      brVerdict rep `shouldBe` "licensed-with-runtime-checks"

    it "structural mismatches outrank licensing" $ do
      let c32 = fnMod "std" "int32" "int32"
          r = fnModArgs "std" ["i64", "int64"] "i64"
          rep = boundaryReport args (Just "c") c32 (typeHeadline c32) (Just "rust") r (typeHeadline r)
      brVerdict rep `shouldBe` "mismatch"

    it "does not consult the dictionary for same-language comparisons" $ do
      -- Mercury's int has no width axiom: against itself there is no
      -- crossing, so the report must stay descriptive.
      let m1 = fnMod "std" "int" "int"
          m2 = fnMod "std" "int" "int"
          rep = boundaryReport args (Just "mercury") m1 (typeHeadline m1) (Just "mercury") m2 (typeHeadline m2)
      brVerdict rep `shouldBe` "identical"
  where
    -- Build a fn type whose qnames live in a given module, so dictionary
    -- lookups hit the real axiom table: (mod/name ...) -> {std/pure} mod/name.
    fnMod :: Text -> Text -> Text -> A.Value
    fnMod m a r = fnModArgs m [a] r
    fnModArgs :: Text -> [Text] -> Text -> A.Value
    fnModArgs modName argNames resName =
      A.object
        [ "fn"
            A..= A.object
              [ "args"
                  A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= con modName n] | n <- argNames],
                "effect" A..= A.object ["effects" A..= [bareQ "std" "pure"]],
                "result" A..= con modName resName
              ]
        ]
    con :: Text -> Text -> A.Value
    con m n = A.object ["con" A..= A.object ["qname" A..= bareQ m n]]
    bareQ :: Text -> Text -> A.Value
    bareQ m n = A.object ["module" A..= m, "name" A..= A.object ["text" A..= n]]
