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
              ] [] [] Nothing
          plan = planBoundary req
      spVerdict plan `shouldBe` "unlicensed-narrowing"
      -- The cited axioms name both representations and the widths:
      -- the argument position refused (64 does not fit 32), the result
      -- position would have been fine (32 fits 64) — the refusal is
      -- the weakest link, matching organ_check_boundary.
      spReasons plan `shouldSatisfy` any ("does not fit 32 bits" `T.isInfixOf`)
      spReasons plan `shouldSatisfy` any ("fits the wider" `T.isInfixOf`)

    it "refuses a request with no positions" $
      spVerdict (planBoundary (StubRequest "a" "b" [] [] [] Nothing)) `shouldBe` "unlicensed-empty"

    it "plans a dynamic crossing as runtime-checks" $ do
      let req = StubRequest "prolog:factorial/factorial" "haskell:Factorial/factorial"
              [ Position "arg 0" (Member FDynamic Nothing "any") (Member FSigned (Just 64) "ghc-prim/Int#")
              , Position "result" (Member FSigned (Just 64) "ghc-prim/Int#") (Member FDynamic Nothing "any")
              ] [] [] Nothing
      spVerdict (planBoundary req) `shouldBe` "licensed-with-runtime-checks"

  describe "renderCStubs (golden)" $ do
    it "renders the haskell->rust fixture exactly (callee trampoline open)" $ do
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
                   , "  /* C2: trampoline into the callee island's runtime — */"
                   , "  /*    provide the island's real entry point to fill this */"
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
                   , "  /* C2: trampoline into the callee island's runtime — */"
                   , "  /*    provide the island's real entry point to fill this */"
                   , "}"
                   ]

    it "renders refusals as comment-only debris" $ do
      let req = StubRequest "haskell:x" "c:y"
              [ Position "arg 0" (Member FSigned (Just 64) "ghc-prim/Int#") (Member FSigned (Just 32) "std/int32") ] [] [] Nothing
          ls = renderCStubs (planBoundary req)
      ls `shouldSatisfy` all (T.isPrefixOf "//")
      ls `shouldSatisfy` any (T.isInfixOf "STUB REFUSED")

  describe "safeIdent" $ do
    it "sanitizes symbols into C-ABI identifiers" $
      safeIdent "haskell:Factorial/factorial" `shouldBe` "omni_haskell_Factorial_factorial"

  describe "C2: filled callee trampolines" $ do
    it "emits the filled trampoline when the island entry is known" $ do
      let req = fixtureHaskellRust {srCalleeExport = Just "rust_island_factorial"}
          ls = spCalleeSide (planBoundary req)
      ls `shouldSatisfy` any (T.isInfixOf "rust_island_factorial")
      ls `shouldSatisfy` not . any (T.isInfixOf "C2: trampoline")
      ls `shouldSatisfy` not . all (T.isPrefixOf "//")  -- real code, not debris

    it "addresses the island export by its own name (no bridge prefix)" $ do
      let req = fixtureHaskellRust {srCalleeExport = Just "my_own_export"}
          callee = spCalleeSide (planBoundary req)
      -- The bridge namespace (omni_rust_...) must not leak onto the
      -- island entry — that collision was the C2 spike's link error.
      callee `shouldSatisfy` any (T.isInfixOf "return (int64_t) my_own_export(a0);")
      callee `shouldSatisfy` not . any (T.isInfixOf "omni_my_own_export")

    it "appends the GHC RTS contract only for haskell islands" $ do
      let hsReq = fixtureHaskellRust {srCaller = "rust:factorial_rs/factorial", srCallee = "haskell:Factorial/factorial", srCalleeExport = Just "hs_fac"}
          hs = spCalleeSide (planBoundary hsReq)
          rs = spCalleeSide (planBoundary (fixtureCWidened {srCalleeExport = Just "rs_fac"}))
      hs `shouldSatisfy` any (T.isInfixOf "hs_init")
      rs `shouldSatisfy` not . any (T.isInfixOf "hs_init")

    it "keeps the explicit C2 placeholder when the entry is unknown" $ do
      let ls = spCalleeSide (planBoundary fixtureHaskellRust)
      ls `shouldSatisfy` any (T.isInfixOf "C2: trampoline")

  describe "C3: effect-row subset rule" $ do
    it "licenses a pure caller into an effectful callee" $ do
      let req = fixtureHaskellRust {srCallerEffects = ["std/pure"], srCalleeEffects = ["std/core/div"]}
      spVerdict (planBoundary req) `shouldBe` "licensed-lossless"

    it "refuses a caller demanding effects the callee lacks" $ do
      let req = fixtureHaskellRust {srCallerEffects = ["std/core/exn"], srCalleeEffects = ["std/core/div"]}
          plan = planBoundary req
      spVerdict plan `shouldBe` "unlicensed-effect-row"
      spReasons plan `shouldSatisfy` any ("std/core/exn" `T.isInfixOf`)
      spReasons plan `shouldSatisfy` any ("subset" `T.isInfixOf`)

    it "normalizes std/pure to the empty row in subset checks" $ do
      let req = fixtureHaskellRust {srCallerEffects = ["std/pure"], srCalleeEffects = []}
      spVerdict (planBoundary req) `shouldBe` "licensed-lossless"

    it "normalizes the callee's pure row too (nothing below pure)" $ do
      let req = fixtureHaskellRust {srCallerEffects = ["std/core/div"], srCalleeEffects = ["std/pure"]}
      spVerdict (planBoundary req) `shouldBe` "unlicensed-effect-row"

    it "adds the licensed row relation to the marshal notes" $ do
      let req = fixtureHaskellRust {srCallerEffects = ["std/pure"], srCalleeEffects = ["std/core/div", "std/core/exn"]}
          notes = spMarshal (planBoundary req)
      notes `shouldSatisfy` any ("effect row: caller {∅} ⊆ callee {std/core/div, std/core/exn}" `T.isInfixOf`)

    it "refuses an effectful caller into a pure callee even when types are lossless" $ do
      -- The C3 heart: representation licensing is necessary but not
      -- sufficient; the effect rule refuses independently.
      let req = fixtureHaskellRust
            { srCallerEffects = ["std/core/div", "std/core/exn"]
            , srCalleeEffects = ["std/core/div"]
            }
      spVerdict (planBoundary req) `shouldBe` "unlicensed-effect-row"
