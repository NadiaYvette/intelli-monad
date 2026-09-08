{-# LANGUAGE OverloadedStrings #-}

-- | C1/C2 of the Phase C transplant plan (@doc/phase-c-transplant.md@):
-- the stub generator. Pure, diff-able, no compiler invoked.
--
-- A 'StubRequest' describes a crossing the representation dictionary
-- can judge. 'planBoundary' re-runs the licensing (same axioms, same
-- direction rules as @organ_check_boundary@, so the two cannot
-- disagree) and either refuses with cited reasons or yields a
-- 'StubPlan'. 'renderCStubs' renders a plan as C-ABI glue text whose
-- every line is deterministic, so tests can pin it exactly and code
-- review is a diff.
--
-- Direction convention: each 'Position' lists its members in
-- /value-flow order/ — @posFrom@ is where the value comes from,
-- @posTo@ where it goes. Argument positions are therefore constructed
-- caller→callee and the result position callee→caller, exactly as
-- @organ_check_boundary@ licenses them.
--
-- Effect discipline (C3): a crossing must satisfy the effect-row
-- subset rule — the caller's effect row must be a subset of the
-- callee's. A call may add obligations for the callee; it may never
-- demand powers the callee lacks. Pure rows (@std/pure@) normalize to
-- the empty row.
--
-- Scope: scalar numeric crossings emit real conversions; boxed values
-- (arbitrary precision, dynamic) render as @void *@ handles (the C4
-- dictionary adds 'IntelliMonad.Tools.OrganBank.Dictionary.FBigSigned' and
-- 'IntelliMonad.Tools.OrganBank.Dictionary.FBigUnsigned' so an unbounded-
-- domain crossing is refused, not under-modeled). The
-- callee side either leaves the trampoline to fill (when the island's
-- real entry point is unknown) or emits the filled forwarding
-- trampoline when 'srCalleeExport' names it — the C2 spike's finding:
-- bridge symbols and island exports are separate namespaces.
module IntelliMonad.Tools.OrganBank.Stubs
  ( Position (..)
  , StubRequest (..)
  , StubPlan (..)
  , planBoundary
  , renderCStubs
  , normalizeRow
  , safeIdent
  , fixtureHaskellRust
  , fixtureCWidened
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

import IntelliMonad.Tools.OrganBank.Dictionary
  ( Evidence (..)
  , Family (..)
  , Member (..)
  , aggregate
  , license
  )

-- | One value crossing the boundary, in value-flow order.
data Position = Position
  { posLabel :: Text
  , posFrom :: Member
  , posTo :: Member
  }
  deriving (Eq, Show)

-- | A crossing to plan: two named symbols, the positions of their
-- shared signature, the two sides' effect rows (rendered qnames), and
-- optionally the callee island's real entry symbol plus an explicit
-- ABI-adapter target name (C4).
data StubRequest = StubRequest
  { srCaller :: Text
  , srCallee :: Text
  , srPositions :: [Position]
  , srCallerEffects :: [Text]
  , srCalleeEffects :: [Text]
  , srCalleeExport :: Maybe Text
  , srCalleeAdapter :: Maybe Text
    -- ^ Symbol name for the generated ABI adapter (the plain @int64_t@
    -- projection the trampoline calls); @Nothing@ emits no adapter.
  , srEffectMap :: Bool
    -- ^ C3 effect reconciliation: request the generator's effect map
    -- for an effectful callee. Koka islands get the @handle/try@ shim
    -- (exceptions mapped to the wire's status sentinel) and the
    -- adapter forwards to the mapped entry; languages without a
    -- generatable mapping fail closed (@'spEffectMap' = Nothing@).
  }
  deriving (Eq, Show)

-- | The outcome of planning: either the crossing is refused (with the
-- dictionary's cited reasons — the refusal is the feature) or a plan
-- with per-side C lines and the per-position marshal notes.
data StubPlan
  = StubRefused { spVerdict :: Text, spReasons :: [Text] }
  | StubPlan
      { spVerdict :: Text
      , spCallerSide :: [Text]
      , spCalleeSide :: [Text]
      , spMarshal :: [Text]
      , spAdapter :: Maybe [Text]
        -- ^ The C4 ABI-adapter section (projecting the wire's plain
        -- @int64_t@ ABI onto the callee island's real ABI), when
        -- requested and supported. Nothing = no adapter for this plan.
      , spEffectMap :: Maybe [Text]
        -- ^ The C3 effect-map section: the koka-side @handle/try@ shim
        -- text (the island's exceptions map to the wire's status
        -- sentinel) plus the mapped-entry contract, when requested and
        -- supported. Nothing = no effect map for this plan.
      }
  deriving (Eq, Show)

-- | Plan a crossing. Checks, in priority order: the degenerate
-- empty request, the effect-row subset rule (caller ⊆ callee), then
-- the dictionary's weakest-link licensing. When both sides carry
-- effect rows, the marshal notes gain the licensed row relation.
planBoundary :: StubRequest -> StubPlan
planBoundary req =
  let perPos = [(p, license (posFrom p) (posTo p)) | p <- srPositions req]
      verdicts = [v | (_, (v, _)) <- perPos]
      aggregated = aggregate verdicts
      axiomLines = concat [lns | (_, (_, lns)) <- perPos]
      callerRow = normalizeRow (srCallerEffects req)
      calleeRow = normalizeRow (srCalleeEffects req)
      missing = [e | e <- callerRow, e `notElem` calleeRow]
   in case (aggregated, missing) of
        (Nothing, _) ->
          StubRefused "unlicensed-empty"
            ["no positions to license: a crossing needs at least one value flow"]
        (_, _ : _) ->
          StubRefused "unlicensed-effect-row"
            [ "caller requires effects the callee does not provide: " <> T.intercalate ", " missing
            , "effect-row rule: caller's row must be a subset of the callee's — a call may add obligations for the callee, never demand powers the callee lacks"
            , "caller row: " <> showRow callerRow <> " | callee row: " <> showRow calleeRow
            ]
        (Just v, [])
          | "unlicensed" `T.isPrefixOf` v -> StubRefused v axiomLines
          | otherwise ->
              let adapter = emitAdapter req
                  adapterOwned = adapter /= Nothing
               in StubPlan
                { spVerdict = v
                , spCallerSide = callerLines req perPos adapterOwned
                , spCalleeSide = calleeLines req perPos adapterOwned
                , spAdapter = adapter
                , spEffectMap = emitEffectMap req
                , spMarshal =
                    [posLabel p <> ": " <> conversionNote (posFrom p) (posTo p) | (p, _) <- perPos]                       <> [ effectNote (srCallerEffects req) (srCalleeEffects req)
                          | not (null (srCallerEffects req)) || not (null (srCalleeEffects req))
                          ]
                }

-- | Drop pure markers: @std/pure@ (and any qname whose name component
-- is @pure@) is the empty row. The last path component is the name.
normalizeRow :: [Text] -> [Text]
normalizeRow = filter (\e -> T.toLower (last (T.splitOn "/" e)) /= "pure")

-- | Render a normalized row for notes/refusals; the empty row is ∅.
showRow :: [Text] -> Text
showRow [] = "∅"
showRow es = T.intercalate ", " es

-- | The licensed effect-row relation, as a marshal note. Rendered from
-- the /raw/ rows so the pure markers stay visible provenance.
effectNote :: [Text] -> [Text] -> Text
effectNote rawCaller rawCallee =
  "effect row: caller {" <> showRow (normalizeRow rawCaller)
    <> "} ⊆ callee {" <> showRow (normalizeRow rawCallee) <> "}"

-- | Per-position conversion note. Real conversions for the scalar
-- cases C1 covers; box notes for the cases C2 must own.
conversionNote :: Member -> Member -> Text
conversionNote from to = case (mFamily from, mFamily to, mWidth from, mWidth to) of
  (FDynamic, _, _, _) -> "runtime-check shape and range, then cast"
  (_, FDynamic, _, _) -> "runtime-check shape and range, then cast"
  (FBigSigned, FBigSigned, _, _) -> "box swap between island allocators (C4)"
  (FBigUnsigned, FBigUnsigned, _, _) -> "box swap between island allocators (C4)"
  (FBigSigned, FBigUnsigned, _, _) -> "unreachable: the negative domain is refused at plan time"
  (FBigUnsigned, FBigSigned, _, _) -> "zero-extend the nat into the signed bigint box"
  (FSigned, FSigned, Just a, Just b)
    | a == b -> "pass through unchanged"
    | a < b -> "sign-extend " <> bits a <> " to " <> bits b
    | otherwise -> "unreachable: narrowing is refused at plan time"
  (FUnsigned, FUnsigned, Just a, Just b)
    | a == b -> "pass through unchanged"
    | a < b -> "zero-extend " <> bits a <> " to " <> bits b
    | otherwise -> "unreachable: narrowing is refused at plan time"
  (FFloat, FFloat, Just 32, Just 64) -> "convert float to double exactly"
  (_, _, _, _) -> "pass through unchanged"
  where
    bits w = T.pack (show w) <> " bits"

-- | C type for a member. Fixed-width integers map to @stdint.h@;
-- 128-bit and unwidthed values render as boxed handles (C2 owns the
-- representation — emitting @__int128@ would bake in a GCC extension).
cTypeOf :: Member -> Text
cTypeOf = maybe "void *" id . cTypeOfMaybe

-- | The C type a member renders as, when its family has one. 'Nothing'
-- for boxed handles (arbitrary precision / dynamic / unwidthed / over-64
-- fixed widths) — the C2 convention owns those as @void *@
-- (emitting @__int128@ would bake in a GCC extension).
cTypeOfMaybe :: Member -> Maybe Text
cTypeOfMaybe m = case (mFamily m, mWidth m) of
  (FBool, _) -> Just "_Bool"
  (FText, _) -> Just "const char *"
  (FUnit, _) -> Just "void"
  (FSigned, Just w) -> intC "int" w
  (FUnsigned, Just w) -> intC "uint" w
  (FFloat, Just 32) -> Just "float"
  (FFloat, Just 64) -> Just "double"
  _ -> Nothing
  where
    intC p w = case w of
      8 -> Just (p <> "8_t")
      16 -> Just (p <> "16_t")
      32 -> Just (p <> "32_t")
      64 -> Just (p <> "64_t")
      _ -> Nothing -- 128-bit etc: boxed handle, not a C extension type

-- | The type the bridge glue uses at the /callee/ side of a position.
-- Adapter-owned crossings (a C4 adapter was emitted) speak the wire's
-- plain @int64_t@ ABI at every wire-representable position: the
-- adapter — or the effect-map shim — is the component that projects
-- that ABI onto the island's real ABI, so the glue never declares a
-- type the symbol at that name lacks.
--
-- Why not 'cTypeOf': a 63-bit OCaml member or a bounded big falls
-- through 'cTypeOf' to @void *@ while the adapter's entry is
-- unconditionally @int64_t@ — glue typed @void *@ against an @int64_t@
-- symbol links by x86-64 ABI luck, not by contract (the wart the FBig
-- wire probe caught on the trampoline extern; the OCaml gold's
-- callee.c carried the same latent mismatch).
--
-- Without an adapter the callee side is the ABI: glue keeps each
-- member's own C type ('cTypeOf'), so a genuine mismatch is a visible
-- compile error rather than a silently reinterpreted value. Members
-- the wire cannot carry — unbounded bigs, 128-bit, text, floats —
-- keep the boxed-handle / own-type convention even adapter-owned
-- (C2 owns their representation).
calleeTypeOf :: Bool -> Member -> Text
calleeTypeOf adapterOwned m
  | adapterOwned, wireNative m = "int64_t"
  | otherwise = cTypeOf m

-- | The type the caller-side bridge glue uses at a position: the
-- caller's own representation when it has a C type; when the caller's
-- member is wire-native but boxed at C level (a bounded big), the
-- wire's int64 ABI — the license makes the value wire-representable,
-- so the wrapper speaks the wire ABI rather than a handle.
callerTypeOf :: Member -> Text
callerTypeOf m = case cTypeOfMaybe m of
  Just t -> t
  Nothing
    | wireNative m -> "int64_t"
    | otherwise -> "void *"

-- | Members the wire's plain int64 ABI carries without semantic
-- reinterpretation: fixed-width integers up to 64 bits, bigs under
-- a declared bound (the license guarantees sentinel headroom inside 64
-- bits, and the generated checks enforce it), and dynamic members —
-- the C4 convention explicitly routes dynamic values through the
-- adapter with runtime checks ('licensed-with-runtime-checks'), which
-- is itself a declaration that the wire's int64 carries them.
-- Unbounded bigs, text, floats, and over-64 fixed widths are not the
-- wire's int64.
wireNative :: Member -> Bool
wireNative m = case (mFamily m, mWidth m, mBound m) of
  (FSigned, Just w, _) -> w <= 64
  (FUnsigned, Just w, _) -> w <= 64
  (FBigSigned, _, Just _) -> True
  (FBigUnsigned, _, Just _) -> True
  (FDynamic, _, _) -> True
  _ -> False

-- | The member as seen from the callee side of a position: arguments
-- arrive callee-typed ('posTo'), the result leaves callee-typed
-- ('posFrom') — the same side convention as 'signature' and the
-- effect-map guard collection.
calleeMember :: Position -> Member
calleeMember p
  | posLabel p == "result" = posFrom p
  | otherwise = posTo p

-- | C-ABI-safe identifier: every non-alphanumeric becomes @_@, always
-- prefixed so a leading digit cannot happen.
safeIdent :: Text -> Text
safeIdent t = "omni_" <> T.map (\c -> if isAlphaNum c then c else '_') t

-- | Sanitize an island-side symbol without the bridge prefix (island
-- exports are their own namespace).
sanitizeIslandSym :: Text -> Text
sanitizeIslandSym = T.map (\c -> if isAlphaNum c then c else '_')

-- | Strip the @lang:@ prefix from a stub-request qname.
stripLangQ :: Text -> Text
stripLangQ r = T.drop 1 (T.dropWhile (/= ':') r)

-- | Koka's C-symbol mangling of a /module/ name: an underscore doubles
-- (module @factorial_emap@ symbols as @kk_factorial__emap_*@), as the
-- generated header's own declarations show. Value names are a separate
-- rule: hyphens become a single underscore (@island-factorial@ ->
-- @island_factorial@), so mangle only where the right rule applies.
mangleKokaModule :: Text -> Text
mangleKokaModule = T.replace "_" "__"

-- | The caller-side island wrapper. The callee is declared @extern@
-- with the callee's ABI signature; each argument converts per its
-- position note (casts for the scalar cases C1 licenses).
callerLines :: StubRequest -> [(Position, (Text, [Text]))] -> Bool -> [Text]
callerLines req perPos adapterOwned =
  [ "// caller-side island wrapper for " <> srCaller req
  , "// crossing verdict: " <> verdictOf perPos
  , "#include <stdint.h>"
  ]
    <> [signature req adapterOwned True]
    <> body
  where
    callee = safeIdent (srCallee req)
    args = [p | p@Position {} <- srPositions req, isArg p]
    isArg p = posLabel p /= "result"
    result = [p | p <- srPositions req, posLabel p == "result"]
    argNames = ["a" <> T.pack (show i) | i <- [0 :: Int ..]]
    body =
      [ "  extern " <> externRet <> " " <> callee <> "(" <> T.intercalate ", " [calleeTypeOf adapterOwned (posTo p) <> " " <> n | (p, n) <- zip args argNames] <> ");"
      , "  return " <> retCast <> callee <> "(" <> T.intercalate ", " ["(" <> calleeTypeOf adapterOwned (posTo p) <> ") " <> n | (p, n) <- zip args argNames] <> ");"
      , "}"
      ]
    -- The extern declares the callee's ABI: callee result type (posFrom
    -- of the result position) and callee-typed parameters (posTo).
    externRet = case result of
      (p : _) -> calleeTypeOf adapterOwned (posFrom p)
      [] -> "void"
    -- The wrapper returns the caller's type (posTo of the result);
    -- the extern call's value arrives in the callee's type.
    retCast = case result of
      (p : _) -> "(" <> callerTypeOf (posTo p) <> ") "
      [] -> ""

verdictOf :: [(Position, (Text, [Text]))] -> Text
verdictOf perPos = case aggregate [v | (_, (v, _)) <- perPos] of
  Just v -> v
  Nothing -> "unlicensed-empty"

-- | The callee-side island wrapper: the exported symbol matching the
-- @extern@ the caller declared. When the island's real entry point is
-- known ('srCalleeExport'), the trampoline is emitted /filled/ — pure
-- forwarding plus the GHC RTS contract when the island is Haskell.
-- When unknown, the body stays an explicit C2 placeholder: never
-- invent an entry point.
calleeLines :: StubRequest -> [(Position, (Text, [Text]))] -> Bool -> [Text]
calleeLines req perPos adapterOwned =
  [ "// callee-side island wrapper for " <> srCallee req
  , "// crossing verdict: " <> verdictOf perPos
  , "#include <stdint.h>"
  ]
    <> [signature req adapterOwned False]
    <> body
  where
    args = [p | p@Position {} <- srPositions req, isArg p]
    isArg p = posLabel p /= "result"
    result = [p | p <- srPositions req, posLabel p == "result"]
    argNames = ["a" <> T.pack (show i) | (i, _) <- zip [0 :: Int ..] args]
    calleeLang = T.takeWhile (/= ':') (srCallee req)
    export = safeIdentExport <$> srCalleeExport req
    -- The island entry is addressed by its own name (bridge symbols
    -- are a namespace of their own — never prefix an island export).
    safeIdentExport t = T.map (\c -> if isAlphaNum c then c else '_') t
    retCast = case result of
      (p : _) -> "(" <> calleeTypeOf adapterOwned (posFrom p) <> ") "
      [] -> ""
    callLine e =
      "  return " <> retCast <> e <> "(" <> T.intercalate ", " argNames <> ");"
    body = case export of
      Nothing -> [ "  /* C2: trampoline into the callee island's runtime — */"
                 , "  /*    provide the island's real entry point to fill this */"
                 , "}" ]
      Just e ->
        [ "  extern " <> islandRet <> " " <> e <> "(" <> T.intercalate ", " [calleeTypeOf adapterOwned (posTo p) <> " " <> n | (p, n) <- zip args argNames] <> ");"
        , "  /* trampoline: forward to the island's entry " <> e <> " */"
        , callLine e
        , "}"
        ]
        where
          -- The island entry has the callee's ABI: callee-typed params
          -- and the callee's result type (posFrom of the result).
          islandRet = case result of
            (p : _) -> calleeTypeOf adapterOwned (posFrom p)
            [] -> "void"
          <> concat
            [ [ "/* GHC islands: the host must call hs_init before the first crossing and"
              , "   hs_exit after the last; this wrapper assumes that contract. */"
              ]
            | calleeLang == "haskell"
            ]
          <> concat
            [ [ "/* koka islands: the ABI adapter owns the runtime contract — init the RTS"
              , "   (kk_main_start + module init chain) inside its setup entry, then call"
              , "   the island through koka's real kk_integer_t + kk_context_t* ABI. */"
              ]
            | calleeLang == "koka"
            ]

-- | The C4 ABI adapter (callee side): projects the wire's plain
-- @int64_t@ ABI onto the callee island's real ABI, so the generated
-- trampoline never depends on hand-written projection code. Emitted
-- only when the request names an adapter target and the callee
-- language has a known projection; the emitters are total over the
-- two languages the gold loops exercise (koka, haskell).
--
-- Everything is keyed on the /callee language/ of 'srCallee' — not on
-- members — because the projection is a property of the island's
-- runtime, not of the scalar positions (koka's @kk_integer_t@ +
-- @kk_context_t*@, GHC's @StgInt@ via its own generated capi header).
emitAdapter :: StubRequest -> Maybe [Text]
emitAdapter req
  -- Fail closed when a callee-side position is not wire-native: the
  -- adapter's entry is unconditionally the wire's @int64_t@ ABI, so a
  -- crossing whose values it cannot carry (unbounded bigs, dynamic,
  -- 128-bit, text, floats) must get NO adapter — emitting one would
  -- leave glue typed @void *@ against an @int64_t@ symbol, linked by
  -- ABI luck. The boxed-handle glue convention (C2) is the honest
  -- output for those domains.
  | not (all (wireNative . calleeMember) (srPositions req)) = Nothing
  | otherwise = case (calleeLang, srCalleeExport req, srCalleeAdapter req) of
  ("koka", Just entry, Just _) ->
    Just $
      [ "// ABI adapter: koka island (generated, C4)"
      , "// Projects the wire's plain int64_t ABI onto koka's real one --"
      , "// kk_integer_t values plus a kk_context_t* -- and owns the koka"
      , "// runtime contract: kk_main_start + the module init chain before"
      , "// the first crossing, module done before exit. The generated koka"
      , "// module's init/done are statically guarded and idempotent; we"
      , "// guard the RTS start ourselves too."]
      <>
      [ "#include <stdint.h>"
      , "#include <kklib.h>"
      , "/* The generated koka header (koka -c -l) declares the real export"
      , "   and the module init/done pair; include it rather than"
      , "   re-declaring, so this adapter cannot drift from the ABI. */"
      , "#include \"" <> headerName <> "\""
      ]
      <> concat
           [ [ "#include \"" <> mangleKokaModule (kokaModule <> "_emap") <> ".h\"" ]
           | srEffectMap req
             -- The effect map's shim module (compiled by koka from the
             -- generated <module>_emap.kk) declares the mapped entry the
             -- adapter body calls. Koka mangles the module's underscore
             -- in the emitted header name (factorial_emap.kk compiles
             -- to factorial__emap.h), so the include must mangle too.
           ]
      <>
      [ ""
      , "static int omni_kk_rts_up = 0;"
      , "void " <> setupEntry <> "(void) {"
      , "  if (omni_kk_rts_up) return;"
      , "  omni_kk_rts_up = 1;"
      , "  kk_context_t* ctx = kk_main_start(0, NULL);"
      , "  kk_" <> mangleKokaModule kokaModule <> "__init(ctx);"
      ]
      <> concat [ [ "  kk_" <> mangleKokaModule (kokaModule <> "_emap") <> "__init(ctx);" ] | srEffectMap req ]
      <>
      [ "}"
      , ""
      , "void " <> teardownEntry <> "(void) {"
      , "  kk_context_t* ctx = kk_get_context();"
      ]
      <> concat [ [ "  kk_" <> mangleKokaModule (kokaModule <> "_emap") <> "__done(ctx);" ] | srEffectMap req ]
      <>
      [ "  kk_" <> mangleKokaModule kokaModule <> "__done(ctx);"
      , "}"
      , ""
      , "int64_t " <> entry' <> "(int64_t n) {"
      ]
      <> ( if srEffectMap req
             then
               [ "  /* C3 effect map: forwards to the mapped entry -- the koka-side"
               , "     handle/try shim catches the island's exceptions and maps them"
               , "     to the wire's status sentinel (min-int64); see the effect-map"
               , "     section of this plan. The mapped entry already speaks the"
               , "     wire's plain int64_t ABI (koka unboxes int64), so this is a"
               , "     pure forward -- no kk_integer boxing here. */"
               , "  return " <> callTarget <> "(n, kk_get_context());"
               , "}"
               ]
             else
               [ "  kk_context_t* ctx = kk_get_context();"
               , "  return kk_smallint_from_integer("
               , "      " <> callTarget <> "(kk_integer_from_int64(n, ctx), ctx));"
               , "}"
               ]
         )
    where
      entry' = sanitizeIsland entry
      headerName = kokaModule <> ".h"
      setupEntry = "omni_kk_" <> kokaModule <> "_island_init"
      teardownEntry = "omni_kk_" <> kokaModule <> "_island_done"
      -- C3: with the effect map requested, the adapter forwards to the
      -- mapped entry (the shim's handle/try does the mapping) instead
      -- of the island's raw export. The shim lives in the generated
      -- <module>_emap module, so its symbol gains the _emap component.
      callTarget
        | srEffectMap req = "kk_" <> mangleKokaModule (kokaModule <> "_emap") <> "_mapped_" <> sanitizeIsland kokaValue
        | otherwise = "kk_" <> mangleKokaModule kokaModule <> "_" <> sanitizeIsland kokaValue
  ("haskell", Just entry, Just _) ->
    Just $
      [ "// ABI adapter: GHC island (generated, C4)"
      , "#include <stdint.h>"
      , "/* GHC emits a capi header per module (ghc -c keeps it next to the"
      , "   object: <Module>_api.h); include it rather than re-declaring the"
      , "   export, so the target-defined StgInt spelling comes from GHC"
      , "   itself. The RTS contract -- hs_init before the first crossing,"
      , "   hs_exit after the last -- stays with the host, which links via"
      , "   ghc -no-hs-main. */"
      , "#include \"" <> hsModule <> "_api.h\""
      , ""
      , "int64_t " <> sanitizeIsland entry <> "(int64_t n) {"
      , "  return (int64_t) " <> hsModule <> "_" <> hsValue <> "((" <> hsArgType <> ") n);"
      , "}"
      ]
  ("ocaml", Just entry, Just _) ->
    Just $
      [ "// ABI adapter: OCaml island (generated, C4)"
      , "// Projects the wire's plain int64_t ABI onto OCaml's 63-bit tagged"
      , "// int: boxing is Val_long ((n << 1) + 1), unboxing Long_val. The"
      , "// island registers its entry via Callback.register; the adapter"
      , "// fetches the closure with caml_named_value and calls"
      , "// caml_callback_exn: the island's exceptions arrive as an"
      , "// exception-result value instead of terminating the process,"
      , "// and surface as the wire's status sentinel -- OCaml islands"
      , "// join the same sentinel contract as koka's handle/try shim."
      , "// The OCaml RTS is brought up once (caml_main) and lives until"
      , "// process exit -- there is no done call. ABI-proven live 2026-09-07"
      , "// (see organ-bank doc/abi-notes/ocaml.md); the host links via"
      , "// ocamlopt, whose C main wins over the runtime's archive member."
      , "#include <stdint.h>"
      , "#include <caml/mlvalues.h>"
      , "#include <caml/callback.h>"
      , ""
      , "static int omni_oc_rts_up = 0;"
      , "void " <> ocSetupEntry <> "(void) {"
      , "  if (omni_oc_rts_up) return;"
      , "  omni_oc_rts_up = 1;"
      , "  char* argv[2] = { (char*)\"" <> ocModule <> "\", NULL };"
      , "  caml_main(argv);"
      , "}"
      , ""
      , "void " <> ocTeardownEntry <> "(void) {"
      , "  /* OCaml RTS lives until process exit; nothing to tear down. */"
      , "}"
      , ""
      , "int64_t " <> sanitizeIsland entry <> "(int64_t n) {"
      ]
      <> ocGuard
      <>
      [ "  value r = caml_callback_exn(*caml_named_value(\"" <> ocValue <> "\"), Val_long(n));"
      , "  if (Is_exception_result(r)) {"
      , "    /* effect map: the island raised; the wire's status sentinel"
      , "       carries the failure (Is_exception_result cannot collide"
      , "       with a boxed Long_val result -- Val_long's low bits are 01 or 11). */"
      , "    return (-0x7fffffffffffffffLL - 1);"
      , "  }"
      , "  return (int64_t) Long_val(r);"
      , "}"
      ]
    where
      (ocdir, ocValue) = T.breakOnEnd "/" (stripLang (srCallee req))
      ocModule = T.dropEnd 1 ocdir
      ocSetupEntry = "omni_oc_" <> ocModule <> "_island_init"
      ocTeardownEntry = "omni_oc_" <> ocModule <> "_island_done"
      -- C5 bounded contract: when the island's argument declares a
      -- bound, the adapter checks it BEFORE boxing. The check must run
      -- here because Val_long silently drops the top bits of a wider
      -- int64 -- the exact silent truncation the license forbids. The
      -- literal is precomputed (no overflow: bounds <= 62 fit int64).
      ocGuard = case [b | p <- srPositions req, posLabel p /= "result", Just b <- [mBound (posTo p)]] of
        (b : _) ->
          let lit = T.pack (show (2 ^ b :: Integer))
           in [ "  /* C5 bounded contract: the island declared |v| < 2^" <> lit' <> ". */"
              , "  if (n >= " <> lit <> " || n <= 0 - " <> lit <> ") {"
              , "    return (-0x7fffffffffffffffLL - 1); /* wire status sentinel */"
              , "  }"
              ]
          where lit' = T.pack (show b)
        [] -> []
  -- Fail-closed: no export to project onto, no adapter request, or a
  -- language without a known projection keeps the one-line FFI export
  -- convention from C2/C3.
  _ -> Nothing -- other languages keep the one-line FFI export convention
  where
    calleeLang = T.takeWhile (/= ':') (srCallee req)
    sanitizeIsland = sanitizeIslandSym
    -- Strip the "lang:" prefix from the callee qname before deriving
    -- module/value names (koka: and haskell: are not 2 chars).
    stripLang = stripLangQ
    -- koka derives the module name from the source path (run_koka.sh
    -- compiles factorial.kk from the build dir => module "factorial")
    -- and value names replace '-' with '_' in the exported C symbol.
    (kdir, kokaValue) = T.breakOnEnd "/" (stripLang (srCallee req))
    kokaModule = T.dropEnd 1 kdir
    -- GHC: module and name from the callee qname; the island export is
    -- the convention prediction Module_name (islands with custom export
    -- names alias it with a one-line foreign export).
    (hdir, hsValue) = T.breakOnEnd "/" (stripLang (srCallee req))
    hsModule = T.dropEnd 1 hdir
    hsArgType = "HsInt64"

-- | The C3 effect map: for a koka island with @srEffectMap@, emit the
-- koka-side @handle/try@ shim source whose compiled entry has exactly
-- the wire's plain @int64_t@ ABI — the island's exceptions map to the
-- wire's status sentinel inside koka, so the C glue never needs a
-- setjmp/longjmp or error-thread bridge.
--
-- ABI-proven live (2026-09-06, /tmp spike + run_koka_mapped.sh): koka
-- compiles the shim's @mapped-<value>@ to
-- @kk_<module>_mapped_<value>(int64_t.., kk_context_t*)@ — @int64@ is
-- unboxed in koka — and the exception raised by the island's real
-- logic surfaces as the sentinel value. The generated adapter (when
-- requested in the same plan) forwards to this mapped entry.
--
-- C5 bounded marshaling: when any callee-side member declares 'mBound'
-- (the bounded-license contract), the shim's checks run in koka's
-- arbitrary-precision @int@ — exact, no C-side overflow risk — throwing
-- on @|v| >= 2^bound@ so the violation rides the same @handle/try@ →
-- sentinel path as any island exception. ABI-proven live 2026-09-07
-- (/tmp/bprobe2): the checked shim compiles to the same pure wire ABI
-- and a host observes the sentinel for an out-of-bound arg AND an
-- out-of-bound result while in-range calls return real values.
--
-- The checked shim's structure is pinned by example so emitter
-- refactors cannot silently change its shape: nested check functions,
-- the strict-inequality corners, the call consuming its pre-checked
-- argument exactly once, the result check wrapping the call, and the
-- sentinel mapping.
--
-- >>> import IntelliMonad.Tools.OrganBank.Dictionary
-- >>> import qualified Data.Text as T
-- >>> import Data.Maybe (fromMaybe)
-- >>> :{
-- let big b = Member FBigSigned Nothing (Just b) EProbed "koka std/core/integer/bounded"
--     req = StubRequest
--       "koka:factorial_big/big-factorial"
--       "koka:factorial_big_bounded/big-bounded-factorial"
--       [ Position "arg 0" (big 60) (big 60)
--       , Position "result" (big 60) (big 60)
--       ]
--       ["std/core/div"] ["std/core/div", "std/core/exn"]
--       (Just "kk_big_island") (Just "kk_big_island") True
--     shim = T.unlines (fromMaybe [] (spEffectMap (planBoundary req)))
-- in and
--      [ T.isInfixOf "fun bcheck_arg_0(x : int) : <exn> int" shim
--      , T.isInfixOf "fun bcheck_result(x : int) : <exn> int" shim
--      , T.isInfixOf "if (x >= 1152921504606846976 || x <= 0 - 1152921504606846976)" shim
--      , T.isInfixOf "val a0 = bcheck_arg_0(int(n))" shim
--      , T.isInfixOf "val r0 = big-bounded-factorial(a0)" shim
--      , T.isInfixOf "std/num/int64/int64(bcheck_result(r0))" shim
--      , T.isInfixOf "fn(exn) min-int64" shim
--      ]
-- :}
-- True
--
-- Fail-closed: no request, a non-koka callee, or a callee whose real
-- export is unknown yields Nothing — the generator never invents a
-- mapping for a language whose exception ABI it cannot emit.
emitEffectMap :: StubRequest -> Maybe [Text]
emitEffectMap req
  -- Same fail-closed rule as 'emitAdapter': the mapped entry is
  -- unconditionally the wire's int64 ABI, so a non-wire-native
  -- callee-side position gets no shim (glue would be typed void * against an
  -- int64_t symbol).
  | not (all (wireNative . calleeMember) (srPositions req)) = Nothing
  | srEffectMap req, calleeLang == "koka", Just _ <- srCalleeExport req =
      Just $
        [ "// C3 effect map: koka-side shim (generator-emitted)." 
        , "// The island's real logic keeps its honest <div,exn> row; this shim"
        , "// runs it under handle/try and maps any exception to the wire's"
        , "// status sentinel (min-int64) -- the wire's plain int64_t ABI then"
        , "// carries either the value or the status, with no setjmp/longjmp."
        , "//"
        , "// ABI proof: koka compiles mapped-" <> kokaValue <> " (module " <> kokaModule <> "_emap) to"
        , "//   int64_t kk_" <> mangleKokaModule (kokaModule <> "_emap") <> "_mapped_" <> sanitizeIslandSym kokaValue <> "(int64_t, kk_context_t*)"
        , "// (int64 is unboxed in koka). Compile with: koka -c -l <this file>"
        , "// plus the island source, then init both modules' __init before use."
        ]
        <> ( case guards of

               [] ->
                 [ "module " <> kokaModule <> "_emap"
                 , "import " <> kokaModule
                 , "import std/num/int64"
                 , ""
                 , "pub fun mapped-" <> kokaValue <> "(n : int64) : <div,exn> int64"
                 , "  handle/try( fn() int64(" <> kokaValue <> "(int(n))), fn(exn) min-int64 )"
                 ]
               _ ->
                 [ "// C5 bounded contract: guards run in koka's arbitrary-precision int"
                 , "// (exact); a violation throws and maps to the sentinel like any"
                 , "// island exception. ABI proof 2026-09-07: same pure wire entry."
                 , "module " <> kokaModule <> "_emap"
                 , "import " <> kokaModule
                 , "import std/num/int64"
                 , ""
                 , "pub fun mapped-" <> kokaValue <> "(n : int64) : <div,exn> int64"
                 , "  handle/try("
                 , "    fn()"
                 , "      {"
                 ]
                 <> concat [bcheckLines lbl b | (lbl, b) <- guards]
                 <> concat
                      [ [ "        val a0 = bcheck_" <> sanitizeIslandSym lbl <> "(int(n))" ]
                      | (lbl, _) <- guards
                      , lbl /= "result"
                      ]
                 <> [ callLine
                    , "        std/num/int64/int64(" <> wrapResult <> ")"
                    , "      },"
                    , "    fn(exn) min-int64"
                    , "  )"
                 ]
           )
        <> [ ""
           , "pub fun dummy-main()"
           , "  ()"
           , ""
           , "// Mapped wire entry (what the generated adapter forwards to):"
           , "//   int64_t kk_" <> mangleKokaModule (kokaModule <> "_emap") <> "_mapped_" <> sanitizeIslandSym kokaValue <> "(int64_t n, kk_context_t* ctx)"
           , "// Status sentinel: " <> sentinelNote
           ]
  | otherwise = Nothing -- no request, unknown export, or no generatable mapping
  where
    calleeLang = T.takeWhile (/= ':') (srCallee req)
    (kdir, kokaValue) = T.breakOnEnd "/" (stripLangQ (srCallee req))
    kokaModule = T.dropEnd 1 kdir
    sentinelNote = "min-int64 (-9223372036854775808): a mapped "
      <> kokaModule <> "/" <> kokaValue <> " result can never be this value"
    -- C5: callee-side members carrying a declared bound. Args are
    -- callee-typed (posTo), the result is the callee's (posFrom) —
    -- the same side convention as 'signature'.
    guards =
      [ (posLabel p, b)
      | p <- srPositions req
      , let m = if posLabel p == "result" then posFrom p else posTo p
      , Just b <- [mBound m]
      ]
    -- One exact check per guarded position: a nested koka function
    -- throwing on |x| >= 2^bound (bound literal precomputed — koka
    -- rejects a unary minus glued to an integer literal).
    bcheckLines lbl b =
      [ "        fun bcheck_" <> sanitizeIslandSym lbl <> "(x : int) : <exn> int"
      , "          if (x >= " <> boundLit <> " || x <= 0 - " <> boundLit <> ") then throw(\"bound violation: |" <> lbl <> "| >= 2^" <> T.pack (show b) <> "\")"
      , "          else x"
      ]
      where boundLit = T.pack (show (2 ^ b :: Integer))
    -- Guarded arg flow: the wire's single-arg convention (arg 0 as
    -- 'n') feeds the arg's check into 'a0'; an unguarded arg passes
    -- through. The call consumes 'a0' exactly once — the check never
    -- runs twice.
    callArg = case [lbl | (lbl, _) <- guards, lbl /= "result"] of
      (_ : _) -> "a0" -- 'a0' is already the checked value; never re-check
      [] -> "int(n)"
    callLine = "        val r0 = " <> kokaValue <> "(" <> callArg <> ")"
    -- Guarded result flow: the checked call result feeds the result's
    -- check; an unguarded result flows straight to the conversion.
    wrapResult = case [b | ("result", b) <- guards] of
      (_ : _) -> "bcheck_result(r0)"
      [] -> "r0"

-- | Shared signature line, side-aware. The caller wrapper receives
-- /caller-typed/ arguments (posFrom) and returns the caller's result
-- type (posTo of the result position); the callee side defines the
-- ABI: callee-typed parameters (posTo) and the callee's result type
-- (posFrom). The two sides differ exactly where the crossing does.
signature :: StubRequest -> Bool -> Bool -> Text
signature req adapterOwned isCaller =
  let args = [p | p@Position {} <- srPositions req, posLabel p /= "result"]
      result = [p | p <- srPositions req, posLabel p == "result"]
      argNames = ["a" <> T.pack (show i) | i <- [0 :: Int ..]]
      retType = case result of
        (p : _) -> if isCaller then callerTypeOf (posTo p) else calleeTypeOf adapterOwned (posFrom p)
        [] -> "void"
      paramType p = if isCaller then callerTypeOf (posFrom p) else calleeTypeOf adapterOwned (posTo p)
      params = T.intercalate ", " [paramType p <> " " <> n | (p, n) <- zip args argNames]
   in retType <> " " <> safeIdent (if isCaller then srCaller req else srCallee req) <> "(" <> params <> ") {"

-- | Render a plan as diff-able text lines. Refusals render as a
-- comment block so a refused crossing leaves no compilable debris.
renderCStubs :: StubPlan -> [Text]
renderCStubs (StubRefused v reasons) =
  [ "// STUB REFUSED: " <> v
  ]
    <> ["//   " <> r | r <- reasons]
    <> ["// a stub generator must refuse to emit code for unlicensed crossings"]
renderCStubs plan@StubPlan {}
  -- C4: the ABI-adapter section (when emitted) trails the callee side
  -- — the last box to compile, right where the island's own runtime
  -- contract lives. The C3 effect-map section (when emitted) trails
  -- the adapter: it is island-side source, compiled by the island's
  -- own compiler, not by the host's gcc.
  | Just adapterLines <- spAdapter plan =
      spCallerSide plan
        <> [""]
        <> ["// marshal notes:"]
        <> ["//   " <> m | m <- spMarshal plan]
        <> [""]
        <> spCalleeSide plan
        <> [""]
        <> adapterLines
        <> maybe [] (("" :) . ("// ---- effect map (island-side source) ----" :) . ("" :)) (spEffectMap plan)
  | otherwise =
      spCallerSide plan
        <> [""]
        <> ["// marshal notes:"]
        <> ["//   " <> m | m <- spMarshal plan]
        <> [""]
        <> spCalleeSide plan
        <> maybe [] (("" :) . ("// ---- effect map (island-side source) ----" :) . ("" :)) (spEffectMap plan)

-- | Fixture pair 1: Haskell @Int#@ crossing into Rust @i64@ and back.
-- Licensed lossless (64-bit signed both ways). Effectless both sides.
fixtureHaskellRust :: StubRequest
fixtureHaskellRust =
  StubRequest
    { srCaller = "haskell:Factorial/factorial",
      srCallee = "rust:factorial/factorial",
      srPositions =
        [ Position "arg 0" (Member FSigned (Just 64) Nothing ESpec "ghc-prim/Int#") (Member FSigned (Just 64) Nothing ESpec "std/i64"),
          Position "result" (Member FSigned (Just 64) Nothing ESpec "std/i64") (Member FSigned (Just 64) Nothing ESpec "ghc-prim/Int#")
        ],
      srCallerEffects = [],
      srCalleeEffects = [],
      srCalleeExport = Nothing,
      srCalleeAdapter = Nothing,
      srEffectMap = False
    }

-- | Fixture pair 2: C @int32@ widened into Rust @i64@ on the argument,
-- Rust @i32@ returned same-width into C. Aggregate: licensed-widening.
fixtureCWidened :: StubRequest
fixtureCWidened =
  StubRequest
    { srCaller = "c:factorial/factorial",
      srCallee = "rust:factorial/factorial",
      srPositions =
        [ Position "arg 0" (Member FSigned (Just 32) Nothing ESpec "std/int32") (Member FSigned (Just 64) Nothing ESpec "std/i64"),
          Position "result" (Member FSigned (Just 32) Nothing ESpec "std/i32") (Member FSigned (Just 32) Nothing ESpec "std/int32")
        ],
      srCallerEffects = [],
      srCalleeEffects = [],
      srCalleeExport = Nothing,
      srCalleeAdapter = Nothing,
      srEffectMap = False
    }
