{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for the C1 stub generator
-- ("IntelliMonad.Tools.OrganBank.Stubs"): licensing agreement with the
-- dictionary, refusal output, and diff-able renders of the two
-- licensed fixture pairs.
module StubSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import IntelliMonad.Tools.OrganBank.Dictionary (Family (..), Member (..))
import IntelliMonad.Tools.OrganBank.Stubs

spec :: Spec
spec = do
  describe "planBoundary" $ do
    it "licenses the haskell->rust 64-bit fixture as lossless" $
      spVerdict (planBoundary fixtureHaskellRust) `shouldBe` "licensed-lossless"

    it "licenses the c-widened fixture as widening" $
      spVerdict (planBoundary fixtureCWidened) `shouldBe` "licensed-widening"

    it "refuses a narrowing crossing with the cited axioms" $ do
      let req = StubRequest "haskell:Factorial/factorial" "c:factorial/factorial"
              [ Position "arg 0" (Member FSigned (Just 64) "ghc-prim/Int#") (Member FSigned (Just 32) "std/int32")
              , Position "result" (Member FSigned (Just 32) "std/int32") (Member FSigned (Just 64) "ghc-prim/Int#")
              ]
          plan = planBoundary req
      spVerdict plan `shouldBe` "unlicensed-narrowing"
      -- The cited axioms name both representations and the widths:
      -- the argument position refused (64 does not fit 32), the result
      -- position would have been fine (32 fits 64) — the refusal is
      -- the weakest link, matching organ_check_boundary.
      spReasons plan `shouldSatisfy` any ("does not fit 32 bits" `T.isInfixOf`)
      spReasons plan `shouldSatisfy` any ("fits the wider" `T.isInfixOf`)

    it "refuses a request with no positions" $
      spVerdict (planBoundary (StubRequest "a" "b" [])) `shouldBe` "unlicensed-empty"

    it "plans a dynamic crossing as runtime-checks" $ do
      let req = StubRequest "prolog:factorial/factorial" "haskell:Factorial/factorial"
              [ Position "arg 0" (Member FDynamic Nothing "any") (Member FSigned (Just 64) "ghc-prim/Int#")
              , Position "result" (Member FSigned (Just 64) "ghc-prim/Int#") (Member FDynamic Nothing "any")
              ]
      spVerdict (planBoundary req) `shouldBe` "licensed-with-runtime-checks"

  describe "renderCStubs (golden)" $ do
    it "renders the haskell->rust fixture exactly" $ do
      renderCStubs (planBoundary fixtureHaskellRust)
        `shouldBe` [ "// caller-side island wrapper for haskell:Factorial/factorial"
                   , "// crossing verdict: licensed-lossless"
                   , "#include <stdint.h>"
                   , "int64_t omni_haskell_Factorial_factorial(int64_t a0) {"
                   , "  extern int64_t omni_rust_factorial_factorial(int64_t a0);"
                   , "  return (int64_t) omni_rust_factorial_factorial((int64_t) a0);"
                   , "}"
                   , ""
                   , "// marshal notes:"
                   , "//   arg 0: pass through unchanged"
                   , "//   result: pass through unchanged"
                   , ""
                   , "// callee-side island wrapper for rust:factorial/factorial"
                   , "// crossing verdict: licensed-lossless"
                   , "#include <stdint.h>"
                   , "int64_t omni_rust_factorial_factorial(int64_t a0) {"
                   , "  /* C2: trampoline into the callee island's runtime */"
                   , "}"
                   ]

    it "renders the c-widened fixture exactly" $ do
      renderCStubs (planBoundary fixtureCWidened)
        `shouldBe` [ "// caller-side island wrapper for c:factorial/factorial"
                   , "// crossing verdict: licensed-widening"
                   , "#include <stdint.h>"
                   , "int32_t omni_c_factorial_factorial(int32_t a0) {"
                   , "  extern int32_t omni_rust_factorial_factorial(int64_t a0);"
                   , "  return (int32_t) omni_rust_factorial_factorial((int64_t) a0);"
                   , "}"
                   , ""
                   , "// marshal notes:"
                   , "//   arg 0: sign-extend 32 bits to 64 bits"
                   , "//   result: pass through unchanged"
                   , ""
                   , "// callee-side island wrapper for rust:factorial/factorial"
                   , "// crossing verdict: licensed-widening"
                   , "#include <stdint.h>"
                   , "int32_t omni_rust_factorial_factorial(int64_t a0) {"
                   , "  /* C2: trampoline into the callee island's runtime */"
                   , "}"
                   ]

    it "renders refusals as comment-only debris" $ do
      let req = StubRequest "haskell:x" "c:y"
              [ Position "arg 0" (Member FSigned (Just 64) "ghc-prim/Int#") (Member FSigned (Just 32) "std/int32") ]
          ls = renderCStubs (planBoundary req)
      ls `shouldSatisfy` all (T.isPrefixOf "//")
      ls `shouldSatisfy` any (T.isInfixOf "STUB REFUSED")

  describe "safeIdent" $ do
    it "sanitizes symbols into C-ABI identifiers" $
      safeIdent "haskell:Factorial/factorial" `shouldBe` "omni_haskell_Factorial_factorial"
