{-# LANGUAGE OverloadedStrings #-}

-- | The representation dictionary: an axiom table of primitive
-- representations per language, and the licenses those axioms grant to
-- a cross-language call.
--
-- This is the first concrete step of organ-tool-plan Phase C: it makes
-- the compatibility assumptions behind a transplanted call explicit
-- and auditable, without pretending to be a sound cross-language
-- typechecker. The axioms are per-(language, qualified-name) facts
-- about how a language's primitive type is represented; a call is
-- /licensed/ when the axioms prove the caller's values always survive
-- the crossing.
--
-- Every entry carries its source. Widths are bits. The honest entries
-- are the ones with 'Nothing' width: a language whose integer size is
-- implementation-defined simply cannot license a fixed-width crossing.
module IntelliMonad.Tools.OrganBank.Dictionary
  ( Family (..)
  , Member (..)
  , memberOf
  , license
  , licenseBig
  , licenseBigIn
  , aggregate
  ) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Representation family of a primitive.
data Family
  = -- | @{True, False}@, no width question.
    FBool
  | -- | Strings/text.
    FText
  | -- | The unit value.
    FUnit
  | -- | Two's-complement signed integers, 'mWidth' bits unless 'Nothing'.
    FSigned
  | -- | Unsigned integers, 'mWidth' bits unless 'Nothing'.
    FUnsigned
  | -- | Arbitrary-precision signed integers (no meaningful width).
    -- Distinct from 'FBigUnsigned' because the signedness of the
    -- domain still crosses the boundary: a value below zero is real
    -- for a signed bigint and unrepresentable for a Nat.
    FBigSigned
  | -- | Arbitrary-precision /non-negative/ integers (no meaningful
    -- width): natural numbers. Every signed-bigint value is in-range
    -- here only when it is >= 0; the widths rule refuses the negative
    -- half as an overflow-domain failure.
    FBigUnsigned
  | -- | IEEE-754 floating point, 'mWidth' bits unless 'Nothing'.
    FFloat
  | -- | The shim could not pin a representation (@\/any@): the value's
    -- structure is deferred to the language runtime.
    FDynamic
  deriving (Eq, Show)

-- | One dictionary entry: a family, a bit width where that question
-- even makes sense, an optional /declared bound/ for the
-- bounded-marshaling contract (see 'license'), and the citation for
-- the claim.
data Member = Member
  { mFamily :: Family,
    mWidth :: Maybe Int,
    mBound :: Maybe Int,
    -- ^ Declared bound in bits (@|v| < 2^bound@, symmetric). 'Nothing'
    -- means the entry claims its family's full domain. A bound must
    -- be strictly smaller than the width when both are present.
    mNote :: Text
  }
  deriving (Eq, Show)

dyn :: Member
dyn = Member FDynamic Nothing Nothing "dynamic: the shim leaves representation to the runtime"

-- | The axiom table. Key: (lowercased language, rendered qname
-- @\"module/name\"@). Sources are quoted in the notes; only facts that
-- can be defended are here.
table :: Map.Map (Text, Text) Member
table =
  Map.fromList $
    concat
      [ -- C: ISO/IEC 9899 (C17). The minimal widths are 5.2.4.2.1
        -- (CHAR_BIT >= 8, SCHAR_MIN <= -127, SHRT_MIN <= -32767,
        -- INT_MIN <= -32767, LONG_MIN <= -2147483647); the 32/64-bit
        -- values below are the LP64 practice of every target organ-bank
        -- runs on, NOT what the standard alone guarantees — plain int
        -- is only guaranteed >= 16 bits. Exact-width unsigned types are
        -- stdint.h (C17 7.20.1.1).
        [ (("c", "std/int8"), Member FSigned (Just 8) Nothing "ISO C 5.2.4.2.1: signed char >= 8 bits; 8 is universal practice"),
          (("c", "std/int16"), Member FSigned (Just 16) Nothing "ISO C 5.2.4.2.1: short >= 16 bits; 16 is universal practice"),
          (("c", "std/int32"), Member FSigned (Just 32) Nothing "ISO C 5.2.4.2.1 guarantees only >= 16; 32 bits is LP64 practice on all organ-bank targets"),
          (("c", "std/int64"), Member FSigned (Just 64) Nothing "ISO C 5.2.4.2.1 (long) + LP64 practice: 64 bits on all organ-bank targets"),
          (("c", "std/u8"), Member FUnsigned (Just 8) Nothing "C17 7.20.1.1: uint8_t (exact-width; optional in theory, universal in practice)"),
          (("c", "std/u16"), Member FUnsigned (Just 16) Nothing "C17 7.20.1.1: uint16_t"),
          (("c", "std/u32"), Member FUnsigned (Just 32) Nothing "C17 7.20.1.1: uint32_t"),
          (("c", "std/u64"), Member FUnsigned (Just 64) Nothing "C17 7.20.1.1: uint64_t"),
          (("c", "std/f32"), Member FFloat (Just 32) Nothing "ISO C 5.2.4.2.2 (FLT_* limits): IEEE-754 binary32 on all supported targets"),
          (("c", "std/f64"), Member FFloat (Just 64) Nothing "ISO C 5.2.4.2.2 (DBL_* limits): IEEE-754 binary64")
        ],
        -- Haskell: the GHC.Prim docs state that Haskell 98 requires at
        -- least 30 bits for Int and that Int# is a machine integer; on
        -- every currently supported 64-bit target that word is 64 bits.
        [ (("haskell", "ghc-prim/Int#"), Member FSigned (Just 64) Nothing "GHC.Prim docs: Haskell98 requires >= 30 bits; Int# is a machine integer — 64 bits on all supported 64-bit targets"),
          (("haskell", "ghc-prim/Word#"), Member FUnsigned (Just 64) Nothing "GHC.Prim docs: Word# is a machine word — 64 bits on all supported 64-bit targets")
        ],
        -- Rust: fixed by the language reference.
        [ (("rust", "std/i8"), Member FSigned (Just 8) Nothing "Rust reference (type.numeric): i8"),
          (("rust", "std/i16"), Member FSigned (Just 16) Nothing "Rust reference (type.numeric): i16"),
          (("rust", "std/i32"), Member FSigned (Just 32) Nothing "Rust reference (type.numeric): i32"),
          (("rust", "std/i64"), Member FSigned (Just 64) Nothing "Rust reference (type.numeric): i64"),
          (("rust", "std/i128"), Member FSigned (Just 128) Nothing "Rust reference (type.numeric): i128"),
          (("rust", "std/u8"), Member FUnsigned (Just 8) Nothing "Rust reference (type.numeric): u8"),
          (("rust", "std/u16"), Member FUnsigned (Just 16) Nothing "Rust reference (type.numeric): u16"),
          (("rust", "std/u32"), Member FUnsigned (Just 32) Nothing "Rust reference (type.numeric): u32"),
          (("rust", "std/u64"), Member FUnsigned (Just 64) Nothing "Rust reference (type.numeric): u64"),
          (("rust", "std/u128"), Member FUnsigned (Just 128) Nothing "Rust reference (type.numeric): u128"),
          (("rust", "std/f32"), Member FFloat (Just 32) Nothing "Rust reference (type.numeric): f32 is IEEE 754-2008 binary32"),
          (("rust", "std/f64"), Member FFloat (Just 64) Nothing "Rust reference (type.numeric): f64 is IEEE 754-2008 binary64")
        ],
        -- Zig: fixed by the language reference.
        [ (("zig", "std/i64"), Member FSigned (Just 64) Nothing "Zig language reference: i64"),
          (("zig", "std/u64"), Member FUnsigned (Just 64) Nothing "Zig language reference: u64"),
          (("zig", "std/f64"), Member FFloat (Just 64) Nothing "Zig language reference: f64 IEEE-754 double")
        ],
        -- OCaml: the manual (§2.1 Base values) documents the tagged int;
        -- one bit is the pointer tag, 63 usable on 64-bit runtimes.
        [ (("ocaml", "Stdlib/int"), Member FSigned (Just 63) Nothing "OCaml manual §2.1 (values): int holds -2^62..2^62-1 on 64-bit runtimes (63-bit tagged)"),
          (("ocaml", "Stdlib/int/bounded61"), Member FSigned (Just 63) (Just 61) "OCaml Stdlib/int with declared bound |v| < 2^61: leaves sentinel headroom inside the 63-bit tagged representation — the generated adapter checks it at the crossing (C5 bounded-marshaling license; a plain i64 caller into unbounded Stdlib/int is refused as a narrowing)"),
          (("ocaml", "Stdlib/float"), Member FFloat (Just 64) Nothing "OCaml manual §2.1 (values): IEEE 754 double, 53-bit mantissa")
        ],
        -- F#: int = Int32 by the F# spec.
        [ (("fsharp", "FSharp.Core/int"), Member FSigned (Just 32) Nothing "F# spec: int is Int32"),
          (("fsharp", "FSharp.Core/float"), Member FFloat (Just 64) Nothing "F# spec: float is IEEE-754 double")
        ],
        -- Julia: Int64/Float64 fixed by the standard library.
        [ (("julia", "Core/Int64"), Member FSigned (Just 64) Nothing "Julia docs: Int64"),
          (("julia", "Core/Int32"), Member FSigned (Just 32) Nothing "Julia docs: Int32"),
          (("julia", "Core/Float64"), Member FFloat (Just 64) Nothing "Julia docs: Float64 IEEE-754 double")
        ],
        -- Swift: Int is word-sized; every supported target is 64-bit.
        [ (("swift", "Swift/Int"), Member FSigned (Just 64) Nothing "Swift: Int is word-sized, 64 bits on all supported targets"),
          (("swift", "Swift/Double"), Member FFloat (Just 64) Nothing "Swift: Double is IEEE-754 double")
        ],
        -- PureScript: Int is 32-bit by the FFI contract; Number is a double.
        [ (("purescript", "Prim/Int"), Member FSigned (Just 32) Nothing "PureScript docs: Int is a 32-bit integer"),
          (("purescript", "Prim/Number"), Member FFloat (Just 64) Nothing "PureScript docs: Number is IEEE-754 double")
        ],
        -- Koka: `int` is arbitrary precision — unboxed smallints carry a
        -- 63-bit payload (kklib.h: KK_TAG_BITS = 1 over a 64-bit kk_intf_t)
        -- and values beyond spill to heap bigints. The ABI entry models
        -- the fixed-width scalar range: every int64 value is representable,
        -- so i64↔koka licenses losslessly (the spike's domain; the C4
        -- adapter marshals via kk_integer_from_int64 / kk_integer_clamp64).
        -- The *full* value range is modeled separately as FBigSigned
        -- (std/core/integer): values beyond int64 are real for koka, and a
        -- crossing claiming them is refused (unlicensed-representation)
        -- rather than under-modeled.
        [ (("koka", "std/core/int"), Member FSigned (Just 64) Nothing "Koka kklib.h: int is arbitrary precision; unboxed smallint payload is 63 bits (KK_TAG_BITS=1), full int64 range representable via heap bigints — ABI models the i64 range"),
          (("koka", "std/core/int/bounded60"), Member FSigned (Just 64) (Just 60) "Koka std/core/int carried over the wire's unboxed int64 ABI with a DECLARED BOUND |v| < 2^60: both sides honor the range, the generated glue checks it and raises the wire's status sentinel on violation (C5 bounded-marshaling license)"),
          (("koka", "std/core/int/bounded61"), Member FSigned (Just 63) (Just 61) "Koka std/core/int with declared bound |v| < 2^61: fits the unboxed smallint payload (KK_TAG_BITS=1) exactly — the checked contract needs no heap bigint on either side"),
          (("koka", "std/core/integer"), Member FBigSigned Nothing Nothing "Koka kklib.h: int is arbitrary precision (63-bit smallint payload + heap bigints) — the full unbounded domain, distinct from the ABI-modeled i64 range"),
          (("koka", "std/core/float64"), Member FFloat (Just 64) Nothing "Koka docs: float64 IEEE-754 double")
        ],
        -- Fortran: gfortran default INTEGER is kind=4 (32 bits); the
        -- standard only guarantees the default kind exists.
        [ (("fortran", "std/integer"), Member FSigned (Just 32) Nothing "GNU Fortran: default INTEGER kind=4 (32 bits)"),
          (("fortran", "std/real"), Member FFloat (Just 32) Nothing "GNU Fortran: default REAL kind=4 (IEEE single)")
        ],
        -- Implementation-defined widths: honestly 'Nothing'.
        [ (("sml", "Basis/int"), Member FSigned Nothing Nothing "SML Basis: Int precision is implementation-defined (SML/NJ 31, MLton 63)"),
          (("mercury", "std/int"), Member FSigned Nothing Nothing "Mercury library: int is implementation-defined (>= 31 bits)")
        ],
        -- C++: ISO C++ guarantees long >= 32 bits; every LP64 target
        -- organ-bank runs on makes it 64. The corpus cpp example emits
        -- std/long for factorial's argument and result.
        [ (("cpp", "std/long"), Member FSigned (Just 64) Nothing "ISO C++: long is 64 bits on LP64 targets")
        ],
        -- Ada: the corpus ada example emits Standard/Integer; GNAT
        -- defines it as 32 bits (RM B.1).
        [ (("ada", "Standard/Integer"), Member FSigned (Just 32) Nothing "GNAT: Integer is 32 bits (Ada RM B.1)")
        ],
        -- Arbitrary precision. Lean/Agda Nat are genuinely unsigned:
        -- the FBigUnsigned family lets the axiom table refuse a signed
        -- bigint source (negatives do not transfer) as overflow-domain
        -- rather than pretending the families are equivalent.
        [ (("lean4", "Lean/Nat"), Member FBigUnsigned Nothing Nothing "Lean 4: Nat is arbitrary precision and non-negative"),
          (("agda", "Agda.Builtin.Nat/Nat"), Member FBigUnsigned Nothing Nothing "Agda: Nat is arbitrary precision and non-negative")
        ],
        -- Canonical core: the shared primitives. Attributed to the
        -- organ-ir shim convention rather than any language standard —
        -- no corpus example emits them yet — so bool/unit/text
        -- crossings can be licensed without each shim inventing its
        -- own module. A shim that names, say, "std/bool" instead still
        -- needs its own entry; the core names are the agreement point.
        [ (("core", "core/bool"), Member FBool Nothing Nothing "organ-ir shim convention: canonical boolean"),
          (("core", "core/unit"), Member FUnit Nothing Nothing "organ-ir shim convention: canonical unit"),
          (("core", "core/text"), Member FText Nothing Nothing "organ-ir shim convention: canonical text")
        ]
      ]

-- | Resolve a (language, module, name) triple. The dynamic marker — a
-- qname rendering to @.../any@ — is recognized for every language,
-- since every shim uses it for values it could not pin down.
--
-- >>> mFamily <$> memberOf "c" "std" "int32"
-- Just FSigned
-- >>> mWidth <$> memberOf "haskell" "ghc-prim" "Int#"
-- Just (Just 64)
-- >>> mWidth <$> memberOf "sml" "Basis" "int"   -- honest unknown
-- Just Nothing
-- >>> memberOf "c" "std" "int24"                -- fail closed
-- Nothing
-- >>> mFamily <$> memberOf "lua" "std" "any"    -- dynamic marker
-- Just FDynamic
-- >>> mFamily <$> memberOf "Haskell" "ghc-prim" "Int#"  -- case-insensitive
-- Just FSigned
memberOf :: Text -> Text -> Text -> Maybe Member
memberOf lang mdl nm
  | nm == "any" = Just dyn
  | otherwise = Map.lookup (T.toLower lang, mdl <> "/" <> nm) table

-- | The verdict a pair of axioms grants, with the cited axioms.
--
--   * @licensed-lossless@ — every caller value round-trips.
--   * @licensed-widening@ — the callee's representation contains the
--     caller's whole range (widening only; narrowing is never licensed).
--   * @licensed-with-runtime-checks@ — one side is dynamic: no static
--     axiom exists, but the crossing is admissible if the generated
--     stub checks at runtime.
--   * @unlicensed-*@ — the crossing can lose information; the stub
--     generator must refuse (narrowing, overflow domain, unprovable
--     range) or the families simply do not connect.
--
-- The verdict names are stable API — stub generation keys on them —
-- so the examples pin them exactly. Widths interact with signedness:
--
-- >>> let m f w n = Member f w Nothing n
-- >>> let mb f w b n = Member f w (Just b) n
-- >>> fst (license (m FSigned (Just 32) "") (m FSigned (Just 32) ""))
-- "licensed-lossless"
-- >>> fst (license (m FSigned (Just 32) "") (m FSigned (Just 64) ""))
-- "licensed-widening"
-- >>> fst (license (m FSigned (Just 64) "") (m FSigned (Just 32) ""))
-- "unlicensed-narrowing"
-- >>> fst (license (m FSigned (Just 32) "") (m FUnsigned (Just 64) ""))
-- "unlicensed-overflow-domain"
-- >>> fst (license (m FDynamic Nothing "") (m FSigned (Just 64) ""))
-- "licensed-with-runtime-checks"
-- >>> fst (license (m FSigned (Just 32) "") (m FFloat (Just 64) ""))
-- "unlicensed-family"
-- >>> fst (license (mb FSigned (Just 64) 60 "") (m FSigned (Just 63) ""))
-- "licensed-bounded"
license :: Member -> Member -> (Text, [Text])
license a b = boundGate baseCase
  where
    isInt f = f == FSigned || f == FUnsigned
    isBig f = f == FBigSigned || f == FBigUnsigned
    lossless = ("licensed-lossless", [axiomLine a, axiomLine b, "same family, no range question"])
    -- The bounded-contract gate (C5): declared bounds only. A
    -- crossing with no explicit 'mBound' behaves exactly as it did
    -- before C5 — the width axioms already speak for representable
    -- ranges, and the sentinel-headroom question is only meaningful
    -- when both sides have /agreed/ to a contract smaller than the
    -- wire. Explicit bounds convert narrowings into checked
    -- crossings and annotate licensed crossings with their axiom.
    boundGate base@(v, axioms)
      | not (isInt (mFamily a) && isInt (mFamily b)) = base
      | mBound a == Nothing && mBound b == Nothing = base
      | otherwise = case (mBound a, mBound b) of
          (Just sa, Just sb)
            | "unlicensed" `T.isPrefixOf` v && v /= "unlicensed-narrowing" -> base
            | sa <= sb, "licensed" `T.isPrefixOf` v -> (v, axioms ++ [fitsLine sa])
            | otherwise -> checked sb (mWidth b) axioms
          (Just sa, Nothing)
            | "licensed" `T.isPrefixOf` v -> (v, axioms ++ [fitsLine sa])
            -- Narrowing rescue by the source's own declared domain:
            -- the source type cannot produce out-of-range values, so
            -- the crossing is safe by construction (no runtime check).
            | "unlicensed-narrowing" == v
            , Just w <- mWidth b
            , sa <= capacity (mFamily b) w ->
                ( "licensed-bounded",
                  axioms ++ [T.pack ("source's declared range |v| < 2^" ++ show sa ++ " fits the destination's representable range - honored by construction (no runtime check required)")]
                )
            | otherwise -> base
          -- The demo direction: a caller side with no declared bound
          -- flows into a callee that declared one. Every value must be
          -- checked at the crossing (the source's full range exceeds
          -- the contract); headroom decides admissibility.
          (Nothing, Just sb)
            | "unlicensed" `T.isPrefixOf` v && v /= "unlicensed-narrowing" -> base
            | otherwise -> checked sb (mWidth b) axioms
          _ -> base
      where
        fitsLine sa = T.pack ("source's declared range |v| < 2^" ++ show sa ++ " fits the destination's representable range - honored by construction")
    -- The magnitude a representation of @w@ bits can hold: |v| <= 2^c
    -- for signed (the sentinel corner excluded separately), |v| < 2^w
    -- for unsigned.
    capacity f w = if f == FSigned then w - 1 else w :: Int
    checked sb carrier axs = case carrier of
      Just w
        | sb + 1 < w ->
            ( "licensed-bounded",
              axs
                <> [ T.pack ("declared bound |v| < 2^" ++ show sb ++ ": the generated glue checks it at the crossing; violations surface as the wire's status sentinel, never silent truncation")
                   ]
            )
        | otherwise ->
            ( "unlicensed-bound-headroom",
              axs
                <> [T.pack ("bound 2^" ++ show sb ++ " leaves no sentinel headroom inside the " ++ show w ++ "-bit representation")]
            )
      Nothing ->
        ( "unlicensed-range",
          axs <> ["declared bound but no width axiom on either side; no headroom claim available"]
        )
    baseCase = case (mFamily a, mFamily b) of
      (FDynamic, _) ->
        ("licensed-with-runtime-checks", [axiomLine a, axiomLine b, "dynamic side: values must be checked at the crossing, not trusted"])
      (_, FDynamic) ->
        ("licensed-with-runtime-checks", [axiomLine a, axiomLine b, "dynamic side: values must be checked at the crossing, not trusted"])
      (FBool, FBool) -> lossless
      (FText, FText) -> lossless
      (FUnit, FUnit) -> lossless
      (FBigSigned, FBigSigned) -> lossless
      (FBigSigned, FBigUnsigned) ->
        ( "unlicensed-overflow-domain",
          [axiomLine a, axiomLine b, "signed bigints admit values below zero; the non-negative side cannot represent them"]
        )
      (FBigUnsigned, FBigSigned) -> lossless
      (FBigUnsigned, FBigUnsigned) -> lossless
      -- The bounded-marshaling contract (C5): a bigint side can cross
      -- into a fixed-width side when the crossing /declares/ the range
      -- both sides will honor - caller values outside the bound are a
      -- runtime failure, not silent truncation. Fixed-to-big without a
      -- bound is plain widening (the next arm); with a bound it is a
      -- checkable contract, and the generated glue enforces it (C5).
      (fa, fb) | isBig fa && isInt fb -> licenseBig a b
      -- Caller-side fixed value flowing INTO the callee's declared big
      -- domain (the argument direction): without a bound this is plain
      -- widening (every fixed value is representable in a bigint); with
      -- a declared bound it is a checkable contract, and the generated
      -- glue enforces it (C5).
      (fa, fb) | isInt fa && isBig fb -> case mBound b of
        Just _ -> licenseBigIn a b
        Nothing -> lossless
      (FFloat, FFloat) -> widths
      -- Signed and unsigned integers are one representation family with a
      -- signedness constraint, not different families: the widths rules
      -- below decide, and the failure mode is overflow-domain, not family.
      (fa, fb) | isInt fa && isInt fb -> widths
      _ ->
        ( "unlicensed-family",
          [axiomLine a, axiomLine b, "different families: no axiom connects the representations"]
        )
    widths = case (mWidth a, mWidth b) of
      (Just wa, Just wb)
        | wa == wb ->
            if mFamily a == mFamily b
              then lossless
              else
                ( "unlicensed-overflow-domain",
                  [axiomLine a, axiomLine b, signAxiom "same width, different signedness: " wa]
                )
        | wa < wb ->
            if mFamily a == FSigned && mFamily b == FUnsigned
              then ("unlicensed-overflow-domain", [axiomLine a, axiomLine b, signAxiom "signed to wider unsigned still admits negatives: " wb])
              else
                ( "licensed-widening",
                  [axiomLine a, axiomLine b, T.pack ("narrower (" ++ show wa ++ " bits) fits the wider (" ++ show wb ++ " bits) representation")]
                )
        | otherwise ->
            ("unlicensed-narrowing", [axiomLine a, axiomLine b, T.pack ("wider (" ++ show wa ++ " bits) does not fit " ++ show wb ++ " bits")])
      -- Either side implementation-defined: the range claim cannot be
      -- made, so the crossing is not licensed. This is the honest
      -- failure mode for SML/Mercury ints.
      _ -> ("unlicensed-range", [axiomLine a, axiomLine b, "one side's precision is implementation-defined; no range axiom available"])
    signAxiom pre w =
      T.pack (pre ++ show w ++ "-bit " ++ fromFamily ++ " does not embed into " ++ toFamily)
    fromFamily = if mFamily a == FUnsigned then "unsigned" else "signed"
    toFamily = if mFamily b == FUnsigned then "unsigned" else "signed"

-- | The bounded-marshaling contract for a @bigint -> fixed-width@
-- position (C5). The crossing is admissible only when the bigint side
-- declares a symmetric bound @|v| < 2^boundBits@ that the fixed side
-- can represent with headroom for the sentinel: the bound must fit
-- inside the callee's width with the sentinel value still outside the
-- declared domain. Violations fail closed with a named reason:
--
-- >>> let big b = Member FBigSigned (Just 64) (Just b) "GHC Integer"
-- >>> let fix w = Member FSigned (Just w) Nothing "std/i64"
-- >>> fst (licenseBig (big 60) (fix 64))
-- "licensed-bounded"
-- >>> fst (licenseBig (Member FBigSigned Nothing Nothing "unbounded") (fix 64))
-- "unlicensed-unbounded"
-- >>> fst (licenseBig (big 63) (fix 64))
-- "unlicensed-bound-headroom"
-- >>> fst (licenseBig (big 70) (fix 64))
-- "unlicensed-bound-headroom"
-- >>> fst (licenseBig (Member FBigUnsigned Nothing (Just 60) "Lean Nat") (fix 64))
-- "unlicensed-overflow-domain"
licenseBig :: Member -> Member -> (Text, [Text])
licenseBig big fixed = case (mFamily big, mBound big, mWidth big, mWidth fixed) of
  -- Family first: an unsigned bigint's negative half (the signed fixed
  -- side's domain below zero) is outside any symmetric bound — no
  -- declared range can repair the domain mismatch.
  (FBigUnsigned, _, _, _) ->
    ( "unlicensed-overflow-domain",
      [axiomLine big, axiomLine fixed, "an unsigned bigint's negative half (the signed fixed side's domain below zero) is outside any symmetric bound"]
    )
  (_, Nothing, _, _) ->
    ( "unlicensed-unbounded",
      [axiomLine big, axiomLine fixed, "no declared bound: the crossing has no range both sides agreed to honor"]
    )
  (_, Just bBits, Just wBig, Just wFixed)
    | bBits >= wBig || bBits + 1 >= wFixed ->
        ( "unlicensed-bound-headroom",
          [ axiomLine big,
            axiomLine fixed,
            T.pack ("bound 2^" ++ show bBits ++ " must be strictly inside the fixed side's " ++ show wFixed ++ "-bit domain with the sentinel (2^(" ++ show wFixed ++ "-1)) outside the declared range")
          ]
        )
    | otherwise ->
        ( "licensed-bounded",
          [ axiomLine big,
            axiomLine fixed,
            T.pack ("declared bound |v| < 2^" ++ show bBits ++ ": values outside are a runtime failure (wire sentinel), not silent truncation")
          ]
        )
  -- No width on the fixed side (or on the big side): the headroom
  -- question cannot be asked — fail closed.
  _ ->
    ( "unlicensed-range",
      [axiomLine big, axiomLine fixed, "bound declared but a side has no width axiom; no headroom claim available"]
    )

-- | The inbound direction of the bounded contract: a caller's
-- fixed-width value flows /into/ the callee's declared big domain.
-- Widening regardless of width (every fixed value is representable in
-- a bigint), but the declared bound must still hold with headroom —
-- the contract's whole point is that both sides honor the same range.
-- Fixed→big /without/ a bound is handled by 'license' directly as
-- plain widening; this function only sees bounded callees.
licenseBigIn :: Member -> Member -> (Text, [Text])
licenseBigIn fixed big = case (mBound big, mWidth fixed) of
  (Just bBits, Just wFixed)
    | bBits + 1 >= wFixed ->
        ( "unlicensed-bound-headroom",
          [axiomLine fixed, axiomLine big, T.pack ("declared bound 2^" ++ show bBits ++ " is not strictly inside the fixed side's " ++ show wFixed ++ "-bit domain")]
        )
    | otherwise ->
        ( "licensed-bounded",
          [axiomLine fixed, axiomLine big, T.pack ("declared bound |v| < 2^" ++ show bBits ++ ": widening holds and the caller's values are checked against the bound at the crossing")]
        )
  _ ->
    ( "unlicensed-range",
      [axiomLine fixed, axiomLine big, "bound declared but the fixed side has no width axiom; no headroom claim available"]
    )

axiomLine :: Member -> Text
axiomLine m =
  "axiom: " <> familyName (mFamily m) <> maybe "" (T.pack . (" (" ++) . (++ " bits)") . show) (mWidth m)
    <> maybe "" (T.pack . (" [|v| < 2^" ++) . (++ "]") . show) (mBound m)
    <> if T.null (mNote m) then "" else " — " <> mNote m
  where
    familyName f = case f of
      FBool -> "bool"
      FText -> "text"
      FUnit -> "unit"
      FSigned -> "signed-int"
      FUnsigned -> "unsigned-int"
      FBigSigned -> "arbitrary-precision-int"
      FBigUnsigned -> "arbitrary-precision-nat"
      FFloat -> "float"
      FDynamic -> "dynamic"

-- | Weakest-link aggregation over the per-position verdicts of one
-- call. Ranks: lossless < widening < runtime-checks < any unlicensed.
-- An empty list means no position was resolvable and the caller must
-- fall back to the descriptive report.
--
-- >>> aggregate []
-- Nothing
-- >>> aggregate ["licensed-lossless", "licensed-widening"]
-- Just "licensed-widening"
-- >>> aggregate ["licensed-widening", "licensed-with-runtime-checks"]
-- Just "licensed-with-runtime-checks"
-- >>> aggregate ["licensed-lossless", "licensed-bounded"]
-- Just "licensed-bounded"
-- >>> aggregate ["licensed-lossless", "unlicensed-narrowing", "licensed-widening"]
-- Just "unlicensed-narrowing"
-- >>> aggregate ["licensed-lossless", "something-new"]   -- unknown fails closed
-- Just "something-new"
aggregate :: [Text] -> Maybe Text
aggregate [] = Nothing
aggregate verdicts = Just $ foldl' weakest "licensed-lossless" verdicts
  where
    rank v
      | v == "licensed-lossless" = (0 :: Int)
      | v == "licensed-widening" = 1
      | v == "licensed-bounded" = 2
      | v == "licensed-with-runtime-checks" = 3
      | "unlicensed" `T.isPrefixOf` v = 4
      | otherwise = 4 -- unknown verdicts fail closed
    weakest acc v = if rank v >= rank acc then v else acc
