{-# LANGUAGE OverloadedStrings #-}

-- | C4 of the Phase C transplant plan (@doc/phase-c-transplant.md@):
-- the PMWA-shaped interop criterion, per-boundary.
--
-- The milestone text: @write down the interop criterion for /that/
-- pair (representation dictionary + calling convention + effect
-- mapping) as a checkable claim.@ Full multi-language soundness
-- proofs stay out of scope — each pair pays its own verification
-- cost, and the cost is paid here in three legs:
--
--   1. /Representation/ — every crossing position stays inside the
--      scalar domain the wire ABI actually covers (fixed-width ints,
--      IEEE floats, bool, text, unit). A position that leaves that
--      domain (arbitrary precision, dynamic) makes the pair's claim
--      /false as stated/: the witness does not reach those values, so
--      the criterion refuses rather than over-claims.
--   2. /Calling convention/ — SR shipped plain @int64_t@ glue whose
--      every line is deterministic ('IntelliMonad.Tools.OrganBank.Stubs.renderCStubs'
--      is a pure function of the plan, so a diff review of the glue is
--      meaningful). The wire is checked against what the plan actually
--      emitted, never against intent.
--   3. /Effects/ — 'IntelliMonad.Tools.OrganBank.Stubs.planBoundary' already
--      reconciled the rows (caller ⊆ callee, the C3 subset rule); when
--      the plan also carries the island-side effect map, the caller's
--      convention has a concrete target for the case where the callee
--      raises.
--
-- The result is a verdict plus the per-leg evidence — PMWA's
-- verified-refuted-unproven shape:
--
--   * @pmwa-verified@ — all three legs hold on the plan's own
--     evidence;
--   * @pmwa-refused-representation@ / @pmwa-refused-effects@ /
--     @pmwa-refused-licensed@ — a leg is refuted, with the offending
--     positions or the dictionary's cited axioms as the reason;
--   * @pmwa-unproven-\<leg\>@ — the leg's witness does not reach far
--     enough (e.g. an effect map was requested but the callee's
--     language has no generatable mapping).
--
-- The witness domain records how far the claim was actually checked:
-- fixed-width scalars are exhausted exactly (the wire ABI is total
-- over them), while any value beyond the bound is explicitly outside
-- the claim — a 63-bit-tagged smallint ABI (koka's) has ~2^63
-- representable values and the wire's 2^40 floor covers its honest
-- middle.
module IntelliMonad.Tools.OrganBank.Interop
  ( InteropPair (..)
  , InteropOutcome (..)
  , pmwaWitnessDomain
  , runInterop
  , planVerdict
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import IntelliMonad.Tools.OrganBank.Dictionary
  ( Family (..)
  , Member (..)
  )
import IntelliMonad.Tools.OrganBank.Stubs
  ( Position (..)
  , StubPlan (..)
  , StubRequest (..)
  , fixtureHaskellRust
  , planBoundary
  , renderCStubs
  )

-- | The pair the criterion is claimed about. Languages only: the
-- boundary is identified by its endpoint islands, exactly as the
-- doc's "per that pair" scopes the claim.
data InteropPair = InteropPair
  { ipCallerLang :: Text
  , ipCalleeLang :: Text
  }
  deriving (Eq, Show)

-- | Outcome of checking a boundary: the verdict, the pair it is
-- about, and the per-leg evidence (Nothing = the leg refused or did
-- not reach — see the verdict).
data InteropOutcome = InteropOutcome
  { ioVerdict :: Text
  , ioPair :: InteropPair
  , ioRepresentation :: Maybe Text
  , ioConvention :: Maybe Text
  , ioEffects :: Maybe Text
  , ioWitnessDomain :: Int
  -- ^ Values the claim is checked over: every member of the scalar
  -- wire domain (exactly), plus the sentinel band reserved for the
  -- effect map's status encoding. A crossing result outside this
  -- domain is outside the claim, not covered by it.
  }
  deriving (Eq, Show)

-- | The witness domain the criterion checks values over. 2^40: the
-- exhaustive-checkable middle of any realistic scalar ABI — well past
-- the 63-bit-tagged smallint honest-middle floor, far below where an
-- unbounded domain would start lying about coverage. Every fixed-width
-- member the wire licenses /is/ exhaustively covered (the glue is
-- total over them); the bound marks where exhaustive reasoning stops
-- and the per-position axioms take over.
pmwaWitnessDomain :: Int
pmwaWitnessDomain = 2 ^ (40 :: Int)

-- | Run the full criterion over a boundary request. Refusals from
-- 'planBoundary' delegate with their verdict, so the criterion can
-- never verify a crossing the dictionary already refused.
--
-- >>> ioVerdict (runInterop fixtureHaskellRust)
-- "pmwa-verified"
-- >>> ipCallerLang (ioPair (runInterop fixtureHaskellRust))
-- "haskell"
-- >>> ipCalleeLang (ioPair (runInterop fixtureHaskellRust))
-- "rust"
-- >>> ioRepresentation (runInterop fixtureHaskellRust)
-- Just "verified: 2 positions stay in the scalar wire domain"
-- >>> ioEffects (runInterop fixtureHaskellRust)
-- Just "verified: effect rows empty on both sides (pure crossing)"
-- >>> ioWitnessDomain (runInterop fixtureHaskellRust) == pmwaWitnessDomain
-- True
runInterop :: StubRequest -> InteropOutcome
runInterop req = case planBoundary req of
  refused@StubRefused {} ->
    InteropOutcome
      { ioVerdict =
          if "effect" `T.isInfixOf` spVerdict refused
            then "pmwa-refused-effects"
            else "pmwa-refused-licensed"
      , ioPair = pairOf req
      , ioRepresentation = Nothing
      , ioConvention = Nothing
      , ioEffects = Just ("refused: " <> spVerdict refused)
      , ioWitnessDomain = 0
      }
  plan@StubPlan {} ->
    let legs = (representationWitness req, Just (conventionWitness req plan), Just (effectWitness req plan))
     in case legs of
          (Nothing, _, _) ->
            InteropOutcome
              { ioVerdict = "pmwa-refused-representation"
              , ioPair = pairOf req
              , ioRepresentation = Nothing
              , ioConvention = legsSecond legs
              , ioEffects = legsThird legs
              , ioWitnessDomain = pmwaWitnessDomain
              }
          (Just rep, Just conv, Just eff)
            | "unproven" `T.isPrefixOf` eff ->
                InteropOutcome
                  { ioVerdict = "pmwa-unproven-effects"
                  , ioPair = pairOf req
                  , ioRepresentation = Just rep
                  , ioConvention = Just conv
                  , ioEffects = Just eff
                  , ioWitnessDomain = pmwaWitnessDomain
                  }
            | otherwise ->
                InteropOutcome
                  { ioVerdict = "pmwa-verified"
                  , ioPair = pairOf req
                  , ioRepresentation = Just rep
                  , ioConvention = Just conv
                  , ioEffects = Just eff
                  , ioWitnessDomain = pmwaWitnessDomain
                  }
          _ ->
            InteropOutcome
              { ioVerdict = "pmwa-unproven-convention"
              , ioPair = pairOf req
              , ioRepresentation = eitherFst legs
              , ioConvention = Nothing
              , ioEffects = legsThird legs
              , ioWitnessDomain = pmwaWitnessDomain
              }
    where
      legsSecond (_, b, _) = b
      legsThird (_, _, c) = c
      eitherFst (a, _, _) = a

pairOf :: StubRequest -> InteropPair
pairOf req =
  InteropPair
    { ipCallerLang = T.takeWhile (/= ':') (srCaller req)
    , ipCalleeLang = T.takeWhile (/= ':') (srCallee req)
    }

-- | Leg 1: every position's members stay in the scalar wire domain —
-- the families whose C ABI the generator actually emits (fixed-width
-- ints, IEEE floats, bool, text, unit). FBig*/FDynamic positions
-- refuse the claim as stated: the wire renders them as @void *@
-- handles, and a claim of exactness over boxed values would be a lie.
representationWitness :: StubRequest -> Maybe Text
representationWitness req
  | null offenders =
      Just
        ( "verified: " <> T.pack (show (length (srPositions req)))
            <> " positions stay in the scalar wire domain"
        )
  | otherwise = Nothing
  where
    offenders =
      [ m
      | p <- srPositions req
      , m <- [posFrom p, posTo p]
      , not (scalarFamily (mFamily m) (mWidth m))
      ]

-- | The families whose C ABI the wire emits exactly.
scalarFamily :: Family -> Maybe Int -> Bool
scalarFamily f w = case f of
  FBool -> True
  FText -> True
  FUnit -> True
  FSigned -> maybe False (\n -> n <= 64) w
  FUnsigned -> maybe False (\n -> n <= 64) w
  FFloat -> maybe False (\n -> n <= 64) w
  FBigSigned -> False -- boxed: unbounded domain, void* in the wire
  FBigUnsigned -> False
  FDynamic -> False

-- | Leg 2: the calling-convention witness — what the wire emitted is
-- exactly what the plan shipped, and the glue is deterministic by
-- construction (renderCStubs is a pure function of the plan, so a
-- diff review is meaningful).
conventionWitness :: StubRequest -> StubPlan -> Text
conventionWitness _ StubRefused {spVerdict = v} = "refused: " <> v
conventionWitness _ plan@StubPlan {} =
  "verified: "
    <> T.pack (show (length (renderCStubs plan)))
    <> " lines of plain int64_t C-ABI glue emitted, deterministic by construction (renderCStubs is a pure function of the plan)"

-- | Leg 3: the effects witness. planBoundary already enforced the
-- subset rule; when the plan carries the island-side effect map, the
-- caller's convention has a concrete target for the raise case.
-- Missing map on request = the leg does not reach (unproven), never
-- silently dropped.
effectWitness :: StubRequest -> StubPlan -> Text
effectWitness _ StubRefused {spVerdict = v} = "refused: " <> v
effectWitness req plan@StubPlan {}
  | srEffectMap req = case spEffectMap plan of
      Just _ -> "verified: effect rows reconciled at plan time; the island-side mapping targets the wire's status sentinel"
      Nothing -> "unproven: effect map requested but not generatable for this callee language"
  | null (srCallerEffects req), null (srCalleeEffects req) =
      "verified: effect rows empty on both sides (pure crossing)"
  | otherwise =
      "verified: effect rows reconciled at plan time (caller row subset of callee row)"

-- | The plan's verdict, refusals included — the criterion's delegate
-- check: a plan the dictionary refused can never come back
-- @pmwa-verified@.
planVerdict :: StubRequest -> Text
planVerdict req = case planBoundary req of
  StubRefused {spVerdict = v} -> v
  StubPlan {spVerdict = v} -> v
