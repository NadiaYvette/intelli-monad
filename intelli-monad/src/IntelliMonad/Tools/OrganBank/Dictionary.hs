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
  | -- | Arbitrary-precision integers (no meaningful width).
    FBigInt
  | -- | IEEE-754 floating point, 'mWidth' bits unless 'Nothing'.
    FFloat
  | -- | The shim could not pin a representation (@\/any@): the value's
    -- structure is deferred to the language runtime.
    FDynamic
  deriving (Eq, Show)

-- | One dictionary entry: a family, a bit width where that question
-- even makes sense, and the citation for the claim.
data Member = Member
  { mFamily :: Family,
    mWidth :: Maybe Int,
    mNote :: Text
  }
  deriving (Eq, Show)

dyn :: Member
dyn = Member FDynamic Nothing "dynamic: the shim leaves representation to the runtime"

-- | The axiom table. Key: (lowercased language, rendered qname
-- @\"module/name\"@). Sources are quoted in the notes; only facts that
-- can be defended are here.
table :: Map.Map (Text, Text) Member
table =
  Map.fromList $
    concat
      [ -- C: ISO/IEC 9899 minimal widths + LP64 practice on the targets
        -- organ-bank runs on.
        [ (("c", "std/int8"), Member FSigned (Just 8) "ISO C: signed char >= 8 bits"),
          (("c", "std/int16"), Member FSigned (Just 16) "ISO C: short >= 16 bits"),
          (("c", "std/int32"), Member FSigned (Just 32) "ISO C: int >= 32 bits on LP64"),
          (("c", "std/int64"), Member FSigned (Just 64) "ISO C: long/long long = 64 bits on LP64"),
          (("c", "std/u8"), Member FUnsigned (Just 8) "ISO C uint8_t"),
          (("c", "std/u16"), Member FUnsigned (Just 16) "ISO C uint16_t"),
          (("c", "std/u32"), Member FUnsigned (Just 32) "ISO C uint32_t"),
          (("c", "std/u64"), Member FUnsigned (Just 64) "ISO C uint64_t"),
          (("c", "std/f32"), Member FFloat (Just 32) "ISO C float: IEEE-754 single"),
          (("c", "std/f64"), Member FFloat (Just 64) "ISO C double: IEEE-754 double")
        ],
        -- Haskell: GHC.AtLeastBoundish Int is documented \">= 2^29-1\"; on
        -- every currently supported 64-bit target Int# is 64 bits.
        [ (("haskell", "ghc-prim/Int#"), Member FSigned (Just 64) "GHC: Int# is 64 bits on all supported 64-bit targets (>= 29 documented)"),
          (("haskell", "ghc-prim/Word#"), Member FUnsigned (Just 64) "GHC: Word# is 64 bits on all supported 64-bit targets")
        ],
        -- Rust: fixed by the language reference.
        [ (("rust", "std/i8"), Member FSigned (Just 8) "Rust reference: i8"),
          (("rust", "std/i16"), Member FSigned (Just 16) "Rust reference: i16"),
          (("rust", "std/i32"), Member FSigned (Just 32) "Rust reference: i32"),
          (("rust", "std/i64"), Member FSigned (Just 64) "Rust reference: i64"),
          (("rust", "std/i128"), Member FSigned (Just 128) "Rust reference: i128"),
          (("rust", "std/u8"), Member FUnsigned (Just 8) "Rust reference: u8"),
          (("rust", "std/u16"), Member FUnsigned (Just 16) "Rust reference: u16"),
          (("rust", "std/u32"), Member FUnsigned (Just 32) "Rust reference: u32"),
          (("rust", "std/u64"), Member FUnsigned (Just 64) "Rust reference: u64"),
          (("rust", "std/u128"), Member FUnsigned (Just 128) "Rust reference: u128"),
          (("rust", "std/f32"), Member FFloat (Just 32) "Rust reference: f32 IEEE-754 single"),
          (("rust", "std/f64"), Member FFloat (Just 64) "Rust reference: f64 IEEE-754 double")
        ],
        -- Zig: fixed by the language reference.
        [ (("zig", "std/i64"), Member FSigned (Just 64) "Zig language reference: i64"),
          (("zig", "std/u64"), Member FUnsigned (Just 64) "Zig language reference: u64"),
          (("zig", "std/f64"), Member FFloat (Just 64) "Zig language reference: f64 IEEE-754 double")
        ],
        -- OCaml: the manual documents 31/63-bit tagged ints (one bit is
        -- the pointer tag) — 63 usable on 64-bit runtimes.
        [ (("ocaml", "Stdlib/int"), Member FSigned (Just 63) "OCaml manual: int is 63-bit signed on 64-bit runtimes (tagged)"),
          (("ocaml", "Stdlib/float"), Member FFloat (Just 64) "OCaml manual: float is IEEE-754 double")
        ],
        -- F#: int = Int32 by the F# spec.
        [ (("fsharp", "FSharp.Core/int"), Member FSigned (Just 32) "F# spec: int is Int32"),
          (("fsharp", "FSharp.Core/float"), Member FFloat (Just 64) "F# spec: float is IEEE-754 double")
        ],
        -- Julia: Int64/Float64 fixed by the standard library.
        [ (("julia", "Core/Int64"), Member FSigned (Just 64) "Julia docs: Int64"),
          (("julia", "Core/Int32"), Member FSigned (Just 32) "Julia docs: Int32"),
          (("julia", "Core/Float64"), Member FFloat (Just 64) "Julia docs: Float64 IEEE-754 double")
        ],
        -- Swift: Int is word-sized; every supported target is 64-bit.
        [ (("swift", "Swift/Int"), Member FSigned (Just 64) "Swift: Int is word-sized, 64 bits on all supported targets"),
          (("swift", "Swift/Double"), Member FFloat (Just 64) "Swift: Double is IEEE-754 double")
        ],
        -- PureScript: Int is 32-bit by the FFI contract; Number is a double.
        [ (("purescript", "Prim/Int"), Member FSigned (Just 32) "PureScript docs: Int is a 32-bit integer"),
          (("purescript", "Prim/Number"), Member FFloat (Just 64) "PureScript docs: Number is IEEE-754 double")
        ],
        -- Koka: std/core/int is 64-bit.
        [ (("koka", "std/core/int"), Member FSigned (Just 64) "Koka docs: int is 64-bit two's complement"),
          (("koka", "std/core/float64"), Member FFloat (Just 64) "Koka docs: float64 IEEE-754 double")
        ],
        -- Fortran: gfortran default INTEGER is kind=4 (32 bits); the
        -- standard only guarantees the default kind exists.
        [ (("fortran", "std/integer"), Member FSigned (Just 32) "GNU Fortran: default INTEGER kind=4 (32 bits)"),
          (("fortran", "std/real"), Member FFloat (Just 32) "GNU Fortran: default REAL kind=4 (IEEE single)")
        ],
        -- Implementation-defined widths: honestly 'Nothing'.
        [ (("sml", "Basis/int"), Member FSigned Nothing "SML Basis: Int precision is implementation-defined (SML/NJ 31, MLton 63)"),
          (("mercury", "std/int"), Member FSigned Nothing "Mercury library: int is implementation-defined (>= 31 bits)")
        ],
        -- C++: ISO C++ guarantees long >= 32 bits; every LP64 target
        -- organ-bank runs on makes it 64. The corpus cpp example emits
        -- std/long for factorial's argument and result.
        [ (("cpp", "std/long"), Member FSigned (Just 64) "ISO C++: long is 64 bits on LP64 targets")
        ],
        -- Ada: the corpus ada example emits Standard/Integer; GNAT
        -- defines it as 32 bits (RM B.1).
        [ (("ada", "Standard/Integer"), Member FSigned (Just 32) "GNAT: Integer is 32 bits (Ada RM B.1)")
        ],
        -- Arbitrary precision.
        [ (("lean4", "Lean/Nat"), Member FBigInt Nothing "Lean 4: Nat is arbitrary precision"),
          (("agda", "Agda.Builtin.Nat/Nat"), Member FBigInt Nothing "Agda: Nat is arbitrary precision")
        ],
        -- Canonical core: the shared primitives. Attributed to the
        -- organ-ir shim convention rather than any language standard —
        -- no corpus example emits them yet — so bool/unit/text
        -- crossings can be licensed without each shim inventing its
        -- own module. A shim that names, say, "std/bool" instead still
        -- needs its own entry; the core names are the agreement point.
        [ (("core", "core/bool"), Member FBool Nothing "organ-ir shim convention: canonical boolean"),
          (("core", "core/unit"), Member FUnit Nothing "organ-ir shim convention: canonical unit"),
          (("core", "core/text"), Member FText Nothing "organ-ir shim convention: canonical text")
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
-- >>> let m f w n = Member f w n
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
license :: Member -> Member -> (Text, [Text])
license a b = case (mFamily a, mFamily b) of
  (FDynamic, _) ->
    ("licensed-with-runtime-checks", [axiomLine a, axiomLine b, "dynamic side: values must be checked at the crossing, not trusted"])
  (_, FDynamic) ->
    ("licensed-with-runtime-checks", [axiomLine a, axiomLine b, "dynamic side: values must be checked at the crossing, not trusted"])
  (FBool, FBool) -> lossless
  (FText, FText) -> lossless
  (FUnit, FUnit) -> lossless
  (FBigInt, FBigInt) -> lossless
  (FFloat, FFloat) -> widths
  -- Signed and unsigned integers are one representation family with a
  -- signedness constraint, not different families: the widths rules
  -- below decide, and the failure mode is overflow-domain, not family.
  (fa, fb) | isInt fa && isInt fb -> widths
  _ ->
    ( "unlicensed-family",
      [axiomLine a, axiomLine b, "different families: no axiom connects the representations"]
    )
  where
    isInt f = f == FSigned || f == FUnsigned
    lossless = ("licensed-lossless", [axiomLine a, axiomLine b, "same family, no range question"])
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

axiomLine :: Member -> Text
axiomLine m =
  "axiom: " <> familyName (mFamily m) <> maybe "" (T.pack . (" (" ++) . (++ " bits)") . show) (mWidth m)
    <> if T.null (mNote m) then "" else " — " <> mNote m
  where
    familyName f = case f of
      FBool -> "bool"
      FText -> "text"
      FUnit -> "unit"
      FSigned -> "signed-int"
      FUnsigned -> "unsigned-int"
      FBigInt -> "arbitrary-precision-int"
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
      | v == "licensed-with-runtime-checks" = 2
      | "unlicensed" `T.isPrefixOf` v = 3
      | otherwise = 3 -- unknown verdicts fail closed
    weakest acc v = if rank v >= rank acc then v else acc
