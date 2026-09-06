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
-- (arbitrary precision, dynamic) render as @void *@ handles. The
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
  , safeIdent
  , fixtureHaskellRust
  , fixtureCWidened
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

import IntelliMonad.Tools.OrganBank.Dictionary
  ( Family (..)
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
-- optionally the callee island's real entry symbol.
data StubRequest = StubRequest
  { srCaller :: Text
  , srCallee :: Text
  , srPositions :: [Position]
  , srCallerEffects :: [Text]
  , srCalleeEffects :: [Text]
  , srCalleeExport :: Maybe Text
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
              StubPlan
                { spVerdict = v
                , spCallerSide = callerLines req perPos
                , spCalleeSide = calleeLines req perPos
                , spMarshal =
                    [posLabel p <> ": " <> conversionNote (posFrom p) (posTo p) | (p, _) <- perPos]
                      <> [ effectNote (srCallerEffects req) (srCalleeEffects req)
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
  (FBigInt, FBigInt, _, _) -> "box swap between island allocators (C2)"
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
cTypeOf m = case (mFamily m, mWidth m) of
  (FBool, _) -> "_Bool"
  (FText, _) -> "const char *"
  (FUnit, _) -> "void"
  (FSigned, Just w) -> intC "int" w
  (FUnsigned, Just w) -> intC "uint" w
  (FFloat, Just 32) -> "float"
  (FFloat, Just 64) -> "double"
  _ -> "void *"
  where
    intC p w = case w of
      8 -> p <> "8_t"
      16 -> p <> "16_t"
      32 -> p <> "32_t"
      64 -> p <> "64_t"
      _ -> "void *"

-- | C-ABI-safe identifier: every non-alphanumeric becomes @_@, always
-- prefixed so a leading digit cannot happen.
safeIdent :: Text -> Text
safeIdent t = "omni_" <> T.map (\c -> if isAlphaNum c then c else '_') t

-- | The caller-side island wrapper. The callee is declared @extern@
-- with the callee's ABI signature; each argument converts per its
-- position note (casts for the scalar cases C1 licenses).
callerLines :: StubRequest -> [(Position, (Text, [Text]))] -> [Text]
callerLines req perPos =
  [ "// caller-side island wrapper for " <> srCaller req
  , "// crossing verdict: " <> verdictOf perPos
  , "#include <stdint.h>"
  ]
    <> [signature req perPos True]
    <> body
  where
    callee = safeIdent (srCallee req)
    args = [p | p@Position {} <- srPositions req, isArg p]
    isArg p = posLabel p /= "result"
    result = [p | p <- srPositions req, posLabel p == "result"]
    argNames = ["a" <> T.pack (show i) | i <- [0 :: Int ..]]
    body =
      [ "  extern " <> externRet <> " " <> callee <> "(" <> T.intercalate ", " [cTypeOf (posTo p) <> " " <> n | (p, n) <- zip args argNames] <> ");"
      , "  return " <> retCast <> callee <> "(" <> T.intercalate ", " ["(" <> cTypeOf (posTo p) <> ") " <> n | (p, n) <- zip args argNames] <> ");"
      , "}"
      ]
    -- The extern declares the callee's ABI: callee result type (posFrom
    -- of the result position) and callee-typed parameters (posTo).
    externRet = case result of
      (p : _) -> cTypeOf (posFrom p)
      [] -> "void"
    -- The wrapper returns the caller's type (posTo of the result);
    -- the extern call's value arrives in the callee's type.
    retCast = case result of
      (p : _) -> "(" <> cTypeOf (posTo p) <> ") "
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
calleeLines :: StubRequest -> [(Position, (Text, [Text]))] -> [Text]
calleeLines req perPos =
  [ "// callee-side island wrapper for " <> srCallee req
  , "// crossing verdict: " <> verdictOf perPos
  , "#include <stdint.h>"
  ]
    <> [signature req perPos False]
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
      (p : _) -> "(" <> cTypeOf (posFrom p) <> ") "
      [] -> ""
    callLine e =
      "  return " <> retCast <> e <> "(" <> T.intercalate ", " argNames <> ");"
    body = case export of
      Nothing -> [ "  /* C2: trampoline into the callee island's runtime — */"
                 , "  /*    provide the island's real entry point to fill this */"
                 , "}" ]
      Just e ->
        [ "  extern " <> islandRet <> " " <> e <> "(" <> T.intercalate ", " [cTypeOf (posTo p) <> " " <> n | (p, n) <- zip args argNames] <> ");"
        , "  /* trampoline: forward to the island's entry " <> e <> " */"
        , callLine e
        , "}"
        ]
        where
          -- The island entry has the callee's ABI: callee-typed params
          -- and the callee's result type (posFrom of the result).
          islandRet = case result of
            (p : _) -> cTypeOf (posFrom p)
            [] -> "void"
          <> concat
            [ [ "/* GHC islands: the host must call hs_init before the first crossing and"
              , "   hs_exit after the last; this wrapper assumes that contract. */"
              ]
            | calleeLang == "haskell"
            ]

-- | Shared signature line, side-aware. The caller wrapper receives
-- /caller-typed/ arguments (posFrom) and returns the caller's result
-- type (posTo of the result position); the callee side defines the
-- ABI: callee-typed parameters (posTo) and the callee's result type
-- (posFrom). The two sides differ exactly where the crossing does.
signature :: StubRequest -> [(Position, (Text, [Text]))] -> Bool -> Text
signature req perPos isCaller =
  let args = [p | p@Position {} <- srPositions req, posLabel p /= "result"]
      result = [p | p <- srPositions req, posLabel p == "result"]
      argNames = ["a" <> T.pack (show i) | i <- [0 :: Int ..]]
      retType = case result of
        (p : _) -> cTypeOf (if isCaller then posTo p else posFrom p)
        [] -> "void"
      paramType p = cTypeOf (if isCaller then posFrom p else posTo p)
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
renderCStubs plan@StubPlan {} =
  spCallerSide plan
    <> [""]
    <> ["// marshal notes:"]
    <> ["//   " <> m | m <- spMarshal plan]
    <> [""]
    <> spCalleeSide plan

-- | Fixture pair 1: Haskell @Int#@ crossing into Rust @i64@ and back.
-- Licensed lossless (64-bit signed both ways). Effectless both sides.
fixtureHaskellRust :: StubRequest
fixtureHaskellRust =
  StubRequest
    { srCaller = "haskell:Factorial/factorial",
      srCallee = "rust:factorial/factorial",
      srPositions =
        [ Position "arg 0" (Member FSigned (Just 64) "ghc-prim/Int#") (Member FSigned (Just 64) "std/i64"),
          Position "result" (Member FSigned (Just 64) "std/i64") (Member FSigned (Just 64) "ghc-prim/Int#")
        ],
      srCallerEffects = [],
      srCalleeEffects = [],
      srCalleeExport = Nothing
    }

-- | Fixture pair 2: C @int32@ widened into Rust @i64@ on the argument,
-- Rust @i32@ returned same-width into C. Aggregate: licensed-widening.
fixtureCWidened :: StubRequest
fixtureCWidened =
  StubRequest
    { srCaller = "c:factorial/factorial",
      srCallee = "rust:factorial/factorial",
      srPositions =
        [ Position "arg 0" (Member FSigned (Just 32) "std/int32") (Member FSigned (Just 64) "std/i64"),
          Position "result" (Member FSigned (Just 32) "std/i32") (Member FSigned (Just 32) "std/int32")
        ],
      srCallerEffects = [],
      srCalleeEffects = [],
      srCalleeExport = Nothing
    }
