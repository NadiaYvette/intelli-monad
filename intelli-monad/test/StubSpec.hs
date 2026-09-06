{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for the C1 stub generator
-- ("IntelliMonad.Tools.OrganBank.Stubs"): licensing agreement with the
-- dictionary, refusal output, and diff-able renders of the two
-- licensed fixture pairs.
module StubSpec (spec) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import IntelliMonad.Tools.OrganBank.Dictionary (Family (..), Member (..), license)
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
              ] [] [] Nothing Nothing False
          plan = planBoundary req
      spVerdict plan `shouldBe` "unlicensed-narrowing"
      -- The cited axioms name both representations and the widths:
      -- the argument position refused (64 does not fit 32), the result
      -- position would have been fine (32 fits 64) — the refusal is
      -- the weakest link, matching organ_check_boundary.
      spReasons plan `shouldSatisfy` any ("does not fit 32 bits" `T.isInfixOf`)
      spReasons plan `shouldSatisfy` any ("fits the wider" `T.isInfixOf`)

    it "refuses a request with no positions" $
      spVerdict (planBoundary (StubRequest "a" "b" [] [] [] Nothing Nothing False)) `shouldBe` "unlicensed-empty"

    it "plans a dynamic crossing as runtime-checks" $ do
      let req = StubRequest "prolog:factorial/factorial" "haskell:Factorial/factorial"
              [ Position "arg 0" (Member FDynamic Nothing "any") (Member FSigned (Just 64) "ghc-prim/Int#")
              , Position "result" (Member FSigned (Just 64) "ghc-prim/Int#") (Member FDynamic Nothing "any")
              ] [] [] Nothing Nothing False
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
              [ Position "arg 0" (Member FSigned (Just 64) "ghc-prim/Int#") (Member FSigned (Just 32) "std/int32") ] [] [] Nothing Nothing False
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

  describe "C4: generated ABI adapters" $ do
    it "emits no adapter unless the request asks for one" $
      spAdapter (planBoundary fixtureHaskellRust) `shouldBe` Nothing

    it "emits no adapter when the island entry is unknown (placeholder path)" $ do
      -- No entry => no trampoline target; an adapter would project onto
      -- a name the plan cannot vouch for.
      let req = fixtureHaskellRust {srCalleeAdapter = Just "some_adapter"}
      spAdapter (planBoundary req) `shouldBe` Nothing

    it "generates the koka adapter with the runtime contract and the real export" $ do
      let req = fixtureHaskellRust
            { srCaller = "rust:factorial_rs/factorial"
            , srCallee = "koka:factorial/island-factorial"
            , srCalleeExport = Just "kk_island_factorial"
            , srCalleeAdapter = Just "kk_island_factorial"
            }
          adapter = T.unpack (T.unlines (fromMaybe [] (spAdapter (planBoundary req))))
      -- The wire ABI is plain int64_t; the projection converts through
      -- kklib's inline helpers and owns the RTS contract.
      adapter `shouldContain` "#include \"factorial.h\""
      adapter `shouldContain` "kk_main_start(0, NULL)"
      adapter `shouldContain` "kk_factorial__init(ctx)"
      adapter `shouldContain` "kk_factorial__done(ctx)"
      adapter `shouldContain` "int64_t kk_island_factorial(int64_t n) {"
      adapter `shouldContain` "kk_factorial_island_factorial(kk_integer_from_int64(n, ctx), ctx)"
      -- Never call the adapter entry recursively by its real-export name:
      adapter `shouldNotContain` "kk_factorial_kk_island_factorial"

    it "generates the GHC adapter against the compiler's capi header" $ do
      let req = fixtureHaskellRust
            { srCaller = "rust:factorial_rs/factorial"
            , srCallee = "haskell:Factorial/factorial"
            , srCalleeExport = Just "hs_island_factorial"
            , srCalleeAdapter = Just "hs_island_factorial"
            }
          adapter = T.unpack (T.unlines (fromMaybe [] (spAdapter (planBoundary req))))
      adapter `shouldContain` "// ABI adapter: GHC island (generated, C4)"
      adapter `shouldContain` "#include \"Factorial_api.h\""
      adapter `shouldContain` "int64_t hs_island_factorial(int64_t n) {"
      -- The convention island entry (Module_name) is the projection's
      -- target; the RTS contract stays with the host.
      adapter `shouldContain` "Factorial_factorial((HsInt64) n)"
      -- The RTS contract stays with the host: the adapter body must not
      -- call hs_init/hs_exit itself (the comment mentions the contract,
      -- so assert on the C call spelling, not the bare token).
      adapter `shouldNotContain` "hs_init("
      adapter `shouldNotContain` "hs_exit("

    it "emits no adapter for languages without a known projection (fail-closed)" $ do
      let req = fixtureHaskellRust {srCalleeAdapter = Just "adapter"}
      spAdapter (planBoundary req) `shouldBe` Nothing

    it "carries the adapter section in renderCStubs output" $ do
      let req = fixtureHaskellRust
            { srCallee = "koka:factorial/island-factorial"
            , srCalleeExport = Just "kk_island_factorial"
            , srCalleeAdapter = Just "kk_island_factorial"
            }
          ls = renderCStubs (planBoundary req)
      ls `shouldSatisfy` any (T.isInfixOf "// ABI adapter: koka island (generated, C4)")
      -- The section trails the callee side.
      let idxAdapter = length (takeWhile (not . T.isInfixOf "// ABI adapter") ls)
          idxCallee = length (takeWhile (not . T.isInfixOf "// callee-side island wrapper") ls)
      idxAdapter `shouldSatisfy` (> idxCallee)

  describe "C4: FBig member families (arbitrary-precision domains)" $ do
    it "licenses same-family bigints as lossless (box swap)" $ do
      -- Nat -> Integer on the argument (zero-extend note), Integer ->
      -- Nat on the result only because THIS crossing is over Nat-typed
      -- values on both sides (FBigUnsigned both ways). The result of a
      -- SIGNED callee into a Nat caller would be overflow-domain.
      let req = StubRequest "lean4:N/Nat" "haskell:GMP/Natural"
              [ Position "arg 0" (Member FBigUnsigned Nothing "Lean/Nat") (Member FBigUnsigned Nothing "GMP/Natural")
              , Position "result" (Member FBigUnsigned Nothing "GMP/Natural") (Member FBigUnsigned Nothing "Lean/Nat")
              ] [] [] Nothing Nothing False
      spVerdict (planBoundary req) `shouldBe` "licensed-lossless"
      spMarshal (planBoundary req) `shouldSatisfy` any ("box swap" `T.isInfixOf`)

    it "licenses Nat into signed bigint on the argument (zero-extend note)" $ do
      let req = StubRequest "lean4:N/Nat" "haskell:GMP/Integer"
              [ Position "arg 0" (Member FBigUnsigned Nothing "Lean/Nat") (Member FBigSigned Nothing "GMP/Integer")
              , Position "result" (Member FBigSigned Nothing "GMP/Integer") (Member FBigSigned Nothing "GMP/Integer")
              ] [] [] Nothing Nothing False
      spVerdict (planBoundary req) `shouldBe` "licensed-lossless"
      spMarshal (planBoundary req) `shouldSatisfy` any ("zero-extend the nat" `T.isInfixOf`)

    it "refuses a signed bigint crossing into a Nat (the negative domain does not transfer)" $ do
      let req = StubRequest "haskell:GMP/Integer" "lean4:N/Nat"
              [ Position "arg 0" (Member FBigSigned Nothing "GMP/Integer") (Member FBigUnsigned Nothing "Lean/Nat") ]
              [] [] Nothing Nothing False
          plan = planBoundary req
      spVerdict plan `shouldBe` "unlicensed-overflow-domain"
      spReasons plan `shouldSatisfy` any ("below zero" `T.isInfixOf`)

    it "refuses bigint-to-fixed-width crossings as a representation failure, not a range guess" $ do
      let req = StubRequest "lean4:N/Nat" "rust:factorial/factorial"
              [ Position "arg 0" (Member FBigSigned Nothing "Lean/Int") (Member FSigned (Just 64) "std/i64")
              , Position "result" (Member FSigned (Just 64) "std/i64") (Member FBigSigned Nothing "Lean/Int")
              ] [] [] Nothing Nothing False
          plan = planBoundary req
      -- Not unlicensed-narrowing (there is no width to compare): the
      -- honest verdict is that the domains are different representations.
      spVerdict plan `shouldBe` "unlicensed-representation"
      spReasons plan `shouldSatisfy` any ("unbounded" `T.isInfixOf`)

    it "renders the FBig crossing refusal as comment-only debris" $ do
      let req = StubRequest "lean4:N/Nat" "rust:factorial/factorial"
              [ Position "arg 0" (Member FBigSigned Nothing "Lean/Int") (Member FSigned (Just 64) "std/i64") ]
              [] [] Nothing Nothing False
      renderCStubs (planBoundary req) `shouldSatisfy` all (T.isPrefixOf "//")

    it "refuses a same-family-but-different-domain FBig pair by domain, not family" $ do
      -- BigSigned -> BigUnsigned is refused (overflow-domain); the
      -- reverse is lossless; a family-blind rule would get both wrong.
      let (vS, _) = license (Member FBigSigned Nothing "a") (Member FBigUnsigned Nothing "b")
          (vU, _) = license (Member FBigUnsigned Nothing "a") (Member FBigSigned Nothing "b")
      vS `shouldBe` "unlicensed-overflow-domain"
      vU `shouldBe` "licensed-lossless"

  describe "C3: the effect map (exception -> caller's convention)" $ do
    it "emits no effect map unless the request asks for one" $
      spEffectMap (planBoundary fixtureHaskellRust) `shouldBe` Nothing
    it "emits no effect map for a non-koka callee (fail-closed)" $ do
      let req = fixtureHaskellRust {srEffectMap = True}
      spEffectMap (planBoundary req) `shouldBe` Nothing
    it "emits the koka handle/try shim with the wire's status sentinel" $ do
      let req = fixtureHaskellRust
            { srCaller = "rust:factorial_rs/factorial"
            , srCallee = "koka:factorial/island-factorial"
            , srCalleeEffects = ["std/core/div", "std/core/exn"]
            , srCalleeExport = Just "kk_island_factorial"
            , srEffectMap = True
            }
          shim = T.unpack (T.unlines (fromMaybe [] (spEffectMap (planBoundary req))))
      -- The shim is koka source in the generated <module>_emap module:
      shim `shouldContain` "module factorial_emap"
      shim `shouldContain` "import factorial"
      shim `shouldContain` "pub fun mapped-island-factorial(n : int64)"
      shim `shouldContain` "handle/try("
      shim `shouldContain` "fn(exn) min-int64"
      -- Deterministic symbol contract, matching the adapter's target
      -- (koka doubles the module name's underscore in C symbols):
      shim `shouldContain` "int64_t kk_factorial__emap_mapped_island_factorial(int64_t n, kk_context_t* ctx)"
      -- Sentinel is stated precisely enough to be checkable:
      shim `shouldContain` "min-int64 (-9223372036854775808)"
    it "targets the adapter at the mapped entry when the map is requested" $ do
      let req = fixtureHaskellRust
            { srCaller = "rust:factorial_rs/factorial"
            , srCallee = "koka:factorial/island-factorial"
            , srCalleeEffects = ["std/core/exn"]
            , srCalleeExport = Just "kk_island_factorial"
            , srCalleeAdapter = Just "kk_island_factorial"
            , srEffectMap = True
            }
          adapter = T.unpack (T.unlines (fromMaybe [] (spAdapter (planBoundary req))))
      -- The mapped entry already speaks plain int64_t (koka unboxes
      -- int64), so the forward is boxing-free:
      adapter `shouldContain` "return kk_factorial__emap_mapped_island_factorial(n, kk_get_context());"
      -- And it must NOT box (no kk_integer conversion in mapped mode):
      adapter `shouldNotContain` "kk_factorial__emap_mapped_island_factorial(kk_integer_from_int64"
    it "trails the adapter section in renderCStubs output" $ do
      let req = fixtureHaskellRust
            { srCallee = "koka:factorial/island-factorial"
            , srCalleeExport = Just "kk_island_factorial"
            , srCalleeAdapter = Just "kk_island_factorial"
            , srEffectMap = True
            }
          ls = renderCStubs (planBoundary req)
          idxOf s = length (takeWhile (not . T.isInfixOf s) ls)
      idxOf "---- effect map" `shouldSatisfy` (> idxOf "// ABI adapter")
