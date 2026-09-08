{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the organ-bank observability tools
-- ("IntelliMonad.Tools.OrganBank"): ingestion of OrganIR JSON into the
-- SQLite index, the query tools, and the boundary-comparison report.
--
-- Every test runs against a fresh index file in a fresh temp directory,
-- so the suite is hermetic and never touches a real @~/.organ@ index.

module OrganSpec (spec) where

import Control.Exception (bracket_)
import Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (openTempFile, hClose)
import Test.Hspec

import IntelliMonad.Tools.OrganBank
import IntelliMonad.Types
import IntelliMonad.Prompt (runPrompt)
import IntelliMonad.Persist (StatelessConf)
import Database.Persist.Sqlite (PersistValue (..))
import System.FilePath (takeFileName)

-- | The two-name sample document used by most tests. Mirrors
-- spec/organ-ir-example.json's shape (haskell, fn type with effect row).
sampleDoc :: A.Value
sampleDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("haskell" :: Text),
            "compiler_version" A..= ("ghc-9.8.4" :: Text),
            "source_file" A..= ("Factorial.hs" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("Factorial" :: Text),
            "exports" A..= ["factorial" :: Text],
            "imports" A..= [q "std/Prelude"],
            "definitions"
              A..= [ def "Factorial" "factorial" "public" (fnTy ["int"] "pure" "int"),
                     def "Factorial" "main" "public" (fnTy [] "io" "unit")
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]
  where
    q :: Text -> A.Value
    q t = A.object ["module" A..= t, "name" A..= A.object ["text" A..= t]]

-- | A Rust document defining the same symbol name — for cross-language
-- lookup and boundary checks.
rustDoc :: A.Value
rustDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("rust" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("factorial_rs" :: Text),
            "definitions"
              A..= [ def "factorial_rs" "factorial" "public" (fnTy ["int"] "io" "int")
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]

-- | A definition with the given module/name/visibility and a fn type
-- (args ->{effects} result). The module mirrors what real shims emit:
-- each definition's name qname carries its own module, and the index
-- keys symbols on it.
def :: Text -> Text -> Text -> A.Value -> A.Value
def m n vis ty =
  A.object
    [ "name" A..= A.object ["module" A..= m, "name" A..= A.object ["text" A..= n, "unique" A..= (1 :: Int)]],
      "type" A..= ty,
      "expr" A..= A.object [],
      "sort" A..= ("fun" :: Text),
      "visibility" A..= vis
    ]

-- | fn type: (args) ->{effects} result. Effect-row elements are bare
-- qnames per the organ-ir schema (verified against spec/examples).
fnTy :: [Text] -> Text -> Text -> A.Value
fnTy argNames eff res =
  A.object
    [ "fn"
        A..= A.object
          [ "args" A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= con a] | a <- argNames],
            "effect" A..= A.object ["effects" A..= [bareQ eff]],
            "result" A..= con res
          ]
    ]
  where
    con t = A.object ["con" A..= A.object ["qname" A..= bareQ t]]
    bareQ t = A.object ["module" A..= ("std" :: Text), "name" A..= A.object ["text" A..= t]]

-- | Run an action with ORGAN_INDEX pointed at a fresh index inside a
-- fresh temp dir, with the sample JSONs written beside it. Restores the
-- env and removes the dir afterwards.
-- | A Haskell document with a PURE definition (empty effect row).
-- Module `FacPure` — distinct from sampleDoc's `Factorial` so the two
-- can share the index without an ambiguous lang-qualified lookup. The
-- fixture for the C4 adapter tests, where the crossing must be
-- licensed before the adapter matters.
haskellPureDoc :: A.Value
haskellPureDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("haskell" :: Text),
            "compiler_version" A..= ("ghc-9.8.4" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("FacPure" :: Text),
            "exports" A..= ["factorial" :: Text],
            "definitions"
              A..= [ def "FacPure" "factorial" "public" (fnTy ["int"] "pure" "int")
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]

-- | A PURE rust document (rustDoc's definition is `io`): the pure
-- caller side for the C4 adapter tests.
rustPureDoc :: A.Value
rustPureDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("rust" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("factorial_pure" :: Text),
            "definitions"
              A..= [ def "factorial_pure" "factorial" "public" (fnTy ["int"] "pure" "int")
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]

kokaDoc :: A.Value
kokaDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("koka" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("factorial" :: Text),
            "definitions"
              A..= [ def "factorial" "island-factorial" "public"
                       (A.object
                          [ "fn" A..= A.object
                              [ "args" A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= kokaCon "std/core" "int"]],
                                "effect" A..= A.object ["effects" A..= [kokaBare "std/core" "div", kokaBare "std/core" "exn"]],
                                "result" A..= kokaCon "std/core" "int"
                              ]
                          ])
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]
  where
    kokaCon :: Text -> Text -> A.Value
    kokaCon m n = A.object ["con" A..= A.object ["qname" A..= kokaBare m n]]
    kokaBare :: Text -> Text -> A.Value
    kokaBare m n = A.object ["module" A..= m, "name" A..= A.object ["text" A..= n]]

-- | The C5 bounded-contract pair: a rust std/i64 caller (pure — the
-- demo direction's real caller type, dictionary-known) and the koka
-- island whose arg/result qname is the dictionary's declared-bound
-- member std/core/int/bounded60 (|v| < 2^60 over the unboxed int64 ABI).
-- The caller module is distinct from 'rustPureDoc' so the two rust
-- fixtures never collide in the shared index.
rustI64Doc :: A.Value
rustI64Doc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("rust" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("factorial_rs_i64" :: Text),
            "definitions"
              A..= [ def "factorial_rs_i64" "factorial" "public"
                       (A.object
                          [ "fn" A..= A.object
                              [ "args" A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= i64Con "std" "i64"]],
                                "effect" A..= A.object ["effects" A..= [i64Bare "std" "pure"]],
                                "result" A..= i64Con "std" "i64"
                              ]
                          ])
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]
  where
    i64Con :: Text -> Text -> A.Value
    i64Con m n = A.object ["con" A..= A.object ["qname" A..= i64Bare m n]]
    i64Bare :: Text -> Text -> A.Value
    i64Bare m n = A.object ["module" A..= m, "name" A..= A.object ["text" A..= n]]

-- | The C5 bounded-contract koka document: same island shape as
-- 'kokaDoc' but its arg/result qname is the dictionary's declared-bound
-- member std/core/int/bounded60 (|v| < 2^60 over the unboxed int64 ABI).
kokaBoundedDoc :: A.Value
kokaBoundedDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("koka" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("factorial" :: Text),
            "definitions"
              A..= [ def "factorial" "bounded-factorial" "public"
                       (A.object
                          [ "fn" A..= A.object
                              [ "args" A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= kokaCon "std/core/int" "bounded60"]],
                                "effect" A..= A.object ["effects" A..= [kokaBare "std/core" "div", kokaBare "std/core" "exn"]],
                                "result" A..= kokaCon "std/core/int" "bounded60"
                              ]
                          ])
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]
  where
    kokaCon :: Text -> Text -> A.Value
    kokaCon m n = A.object ["con" A..= A.object ["qname" A..= kokaBare m n]]
    kokaBare :: Text -> Text -> A.Value
    kokaBare m n = A.object ["module" A..= m, "name" A..= A.object ["text" A..= n]]

-- | The C5 OCaml island document: Stdlib/int/bounded61 (declared
-- bound over the 63-bit tagged int) on arg and result, no effects.
ocamlBoundedDoc :: A.Value
ocamlBoundedDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("ocaml" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("factorial_oc" :: Text),
            "definitions"
              A..= [ def "factorial_oc" "island-factorial" "public"
                       (A.object
                          [ "fn" A..= A.object
                              [ "args" A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= ocCon "Stdlib/int" "bounded61"]],
                                "effect" A..= A.object ["effects" A..= ([] :: [A.Value])],
                                "result" A..= ocCon "Stdlib/int" "bounded61"
                              ]
                          ])
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]
  where
    ocCon :: Text -> Text -> A.Value
    ocCon m n = A.object ["con" A..= A.object ["qname" A..= A.object ["module" A..= m, "name" A..= A.object ["text" A..= n]]]]

-- | The FBig (arbitrary-precision) caller document: koka's unbounded
-- std/core/integer member on arg and result.
kokaBigDoc :: A.Value
kokaBigDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("koka" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("factorial_big" :: Text),
            "definitions"
              A..= [ def "factorial_big" "big-factorial" "public"
                       (A.object
                          [ "fn" A..= A.object
                              [ "args" A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= kokaCon "std/core" "integer"]],
                                "effect" A..= A.object ["effects" A..= [kokaBare "std/core" "div", kokaBare "std/core" "exn"]],
                                "result" A..= kokaCon "std/core" "integer"
                              ]
                          ])
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]
  where
    kokaCon m n = A.object ["con" A..= A.object ["qname" A..= kokaBare m n]]
    kokaBare :: Text -> Text -> A.Value
    kokaBare m n = A.object ["module" A..= m, "name" A..= A.object ["text" A..= n]]

-- | The FBig bounded callee document: std/core/integer/bounded60 —
-- the unbounded big-int domain narrowed by a declared both-sides
-- contract (|v| < 2^60).
kokaBigBoundedDoc :: A.Value
kokaBigBoundedDoc =
  A.object
    [ "schema_version" A..= ("1.0.0" :: Text),
      "metadata"
        A..= A.object
          [ "source_language" A..= ("koka" :: Text),
            "shim_version" A..= ("0.1.0" :: Text)
          ],
      "module"
        A..= A.object
          [ "name" A..= ("factorial_big_bounded" :: Text),
            "definitions"
              A..= [ def "factorial_big_bounded" "big-bounded-factorial" "public"
                       (A.object
                          [ "fn" A..= A.object
                              [ "args" A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= kokaCon "std/core/integer" "bounded60"]],
                                "effect" A..= A.object ["effects" A..= [kokaBare "std/core" "div", kokaBare "std/core" "exn"]],
                                "result" A..= kokaCon "std/core/integer" "bounded60"
                              ]
                          ])
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]
  where
    kokaCon m n = A.object ["con" A..= A.object ["qname" A..= kokaBare m n]]
    kokaBare :: Text -> Text -> A.Value
    kokaBare m n = A.object ["module" A..= m, "name" A..= A.object ["text" A..= n]]

withFreshIndex :: (FilePath -> IO a) -> IO a
withFreshIndex act = do
  tmpRoot <- getTemporaryDirectory
  bracket_ (pure ()) (pure ()) $ do
    withTempDir tmpRoot $ \tmp -> do
      let f1 = tmp ++ "/factorial.json"
          f2 = tmp ++ "/factorial_rs.json"
          f3 = tmp ++ "/factorial_pure.json"
          f4 = tmp ++ "/factorial_kk.json"
          f5 = tmp ++ "/factorial_rs_pure.json"
          f6 = tmp ++ "/factorial_kk_bounded.json"
          f7 = tmp ++ "/factorial_rs_i64.json"
          f8 = tmp ++ "/factorial_oc_bounded.json"
          f9 = tmp ++ "/factorial_big.json"
          f10 = tmp ++ "/factorial_big_bounded.json"
      A.encodeFile f1 sampleDoc
      A.encodeFile f2 rustDoc
      A.encodeFile f3 haskellPureDoc
      A.encodeFile f4 kokaDoc
      A.encodeFile f5 rustPureDoc
      A.encodeFile f6 kokaBoundedDoc
      A.encodeFile f7 rustI64Doc
      A.encodeFile f8 ocamlBoundedDoc
      A.encodeFile f9 kokaBigDoc
      A.encodeFile f10 kokaBigBoundedDoc
      old <- lookupEnv "ORGAN_INDEX"
      setEnv "ORGAN_INDEX" (tmp ++ "/index.db")
      r <- act tmp
      case old of
        Just v -> setEnv "ORGAN_INDEX" v
        Nothing -> unsetEnv "ORGAN_INDEX"
      return r
  where
    -- base-only unique directory: create a temp file, remove it, mkdir
    -- in its place (small race, acceptable for a hermetic test).
    withTempDir root f = do
      (fp, h) <- openTempFile root "organ-test"
      hClose h
      removeFile fp
      createDirectory fp
      r <- f fp
      removeDirectoryRecursive fp
      return r

-- | Read the diagnostics table the way organ_diagnostics does —
-- used to assert what an ingest actually recorded.
readDiagnostics :: FilePath -> IO [(FilePath, Text, Text)]
readDiagnostics idx =
  withIndex idx $ \conn ->
    queryRows conn
      "SELECT source_file, severity, message FROM diagnostics ORDER BY rowid"
      []
      (\r -> return (T.unpack (pvText (r !! 0)), pvText (r !! 1), pvText (r !! 2)))

spec :: Spec
spec = do
  describe "ingestPath" $ do
    it "ingests all fixture documents and reports zero failures" $ do
      (ok, bad, errs) <- withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        ingestPath idx tmp
      ok `shouldBe` 10
      bad `shouldBe` 0
      errs `shouldBe` []

    it "records a diagnostic for malformed JSON" $ do
      (ok, bad, _errs) <- withFreshIndex $ \tmp -> do
        writeFile (tmp ++ "/broken.json") "{not json"
        idx <- defaultOrganIndex
        ingestPath idx tmp
      ok `shouldBe` 10
      bad `shouldBe` 1

  describe "diag envelope ingestion (organ-extract --diag)" $ do
    let envelope files =
          A.object
            [ "organ_diag_version" A..= ("1" :: Text),
              "files" A..= files
            ]
        row :: FilePath -> A.Value -> Text -> A.Value
        row p st err =
          A.object
            [ "path" A..= p,
              "language" A..= A.Null,
              "shim" A..= A.Null,
              "status" A..= st,
              "exit_code" A..= A.Null,
              "stderr" A..= err,
              "warnings" A..= ([] :: [Text])
            ]
        -- Direct-diag variant of withFreshIndex: same temp discipline,
        -- but seeds an envelope instead of module JSONs.
        withDiagIndex files act = do
          tmpRoot <- getTemporaryDirectory
          (fp, h) <- openTempFile tmpRoot "organ-test"
          hClose h
          removeFile fp
          createDirectory fp
          A.encodeFile (fp ++ "/diag.json") (envelope files)
          old <- lookupEnv "ORGAN_INDEX"
          setEnv "ORGAN_INDEX" (fp ++ "/index.db")
          r <- act fp
          case old of
            Just v -> setEnv "ORGAN_INDEX" v
            Nothing -> unsetEnv "ORGAN_INDEX"
          removeDirectoryRecursive fp
          return r
    it "routes envelopes to diagnostics, one row per file, keeping verbatim stderr" $ do
      ((ok, bad, _errs), rows) <- withDiagIndex
        [ row "/src/good.lua" "ok" "",
          row "/src/bad.lua" "error" "Parse error: Unexpected token",
          row "/src/noexe.erl" "error" "erlc-organ could not be run: posix_spawnp"
        ]
        $ \tmp -> do
          idx <- defaultOrganIndex
          r <- ingestPath idx tmp
          rows <- readDiagnostics (tmp ++ "/index.db")
          return (r, rows)
      ok `shouldBe` 1
      bad `shouldBe` 0
      map (\(f, s, m) -> (takeFileName f, s, m)) rows
        `shouldBe`
          [ ("good.lua", "ok", "ok"),
            ("bad.lua", "error", "Parse error: Unexpected token"),
            ("noexe.erl", "error", "erlc-organ could not be run: posix_spawnp")
          ]

    it "parses DiagRow severity from status, keeping stderr verbatim" $
      do
        let v = row "/x/bad.c" "error" ("cpp-organ failed (exit 1) on /x/bad.c: syntax" :: Text)
        case A.fromJSON v of
          A.Success r -> do
            drSeverity r `shouldBe` "error"
            drMessage r `shouldBe` "cpp-organ failed (exit 1) on /x/bad.c: syntax"
          A.Error e -> expectationFailure e
    it "maps ok status with empty stderr to a plain ok row" $
      do
        let v = row "/x/good.c" "ok" ("" :: Text)
        case A.fromJSON v of
          A.Success r -> do
            drSeverity r `shouldBe` "ok"
            drMessage r `shouldBe` "ok"
          A.Error e -> expectationFailure e
    it "maps ok status with non-empty stderr to a warning row" $
      do
        let v = row "/x/warn.c" "ok" ("note: skipped construct" :: Text)
        case A.fromJSON v of
          A.Success r -> do
            drSeverity r `shouldBe` "ok"
            drMessage r `shouldBe` "warning: note: skipped construct"
          A.Error e -> expectationFailure e

  describe "HasFunctionObject" $ do
    it "names the tools in the organ_* namespace" $ do
      getFunctionName @OrganRepoMap `shouldBe` "organ_repo_map"
      getFunctionName @OrganFindSymbol `shouldBe` "organ_find_symbol"
      getFunctionName @OrganCheckBoundary `shouldBe` "organ_check_boundary"
      getFunctionName @OrganDiagnostics `shouldBe` "organ_diagnostics"
      getFunctionName @OrganIngest `shouldBe` "organ_ingest"

  describe "typeHeadline / boundary report (pure parts)" $ do
    it "renders a fn headline" $
      typeHeadline (fnTy ["int"] "pure" "int") `shouldBe` "(std/int) -> {std/pure} std/int"

    it "classifies identical types (descriptive, no languages)" $
      brVerdict (sameArgsNoLang (fnTy ["int"] "pure" "int") (fnTy ["int"] "pure" "int")) `shouldBe` "identical"

    it "refuses identical JSON across different languages (the dictionary's whole point)" $ do
      -- Identical *renderings* in two languages are precisely the
      -- C-int-vs-Rust-i64 illusion: the verdict must fall to the
      -- dictionary, which has no axiom for the fixture's synthetic
      -- std/int qname, and fail closed.
      let r = sameArgs (fnTy ["int"] "pure" "int") (fnTy ["int"] "pure" "int")
      brVerdict r `shouldBe` "unlicensed-boundary"
      any ("outside the dictionary" `T.isInfixOf`) (brDetail r) `shouldBe` True

    it "classifies an effect-row mismatch" $ do
      let r = sameArgs (fnTy ["int"] "pure" "int") (fnTy ["int"] "io" "int")
      brVerdict r `shouldBe` "mismatch"
      any ("Effect rows differ" `T.isPrefixOf`) (brDetail r) `shouldBe` True

    it "classifies an arity mismatch" $
      brVerdict (sameArgs (fnTy ["int"] "pure" "int") (fnTy ["int", "int"] "pure" "int")) `shouldBe` "mismatch"

    it "classifies shape-only differences" $
      brVerdict
        ( sameArgs
            (fnTy ["int"] "pure" "int")
            (A.object ["con" A..= A.object ["module" A..= ("std" :: Text), "name" A..= A.object ["text" A..= ("int" :: Text)]]])
        )
        `shouldBe` "differs-in-shape"

  describe "renderRepoMap (pure)" $ do
    let mods =
          [ ("Factorial", "haskell", "[]", 1),
            ("Factorial", "rust", "[]", 1),
            ("Factorial", "c", "[]", 1)
          ]
        syms =
          [ ("Factorial", "haskell", "factorial", "fun", "public", "(ghc-prim/Int#) -> ghc-prim/Int#"),
            ("Factorial", "rust", "factorial", "fun", "public", "(std/i64) -> std/i64"),
            ("Factorial", "rust", "helper", "fun", "private", "(std/i64) -> std/i64")
          ]
    it "groups symbols per (lang, module), not per bare module name" $ do
      let ls = renderRepoMap mods syms
      ls `shouldContain` ["Factorial [haskell]", "  factorial : fun/public  (ghc-prim/Int#) -> ghc-prim/Int#"]
      -- The rust block must contain only rust's symbols: the old
      -- module-name-only grouping leaked haskell's headline into every
      -- language's block.
      let rustBlock = takeWhile (\l -> not (" [" `T.isInfixOf` l)) (drop 1 (dropWhile (\l -> l /= "Factorial [rust]") ls))
      rustBlock `shouldContain` ["  factorial : fun/public  (std/i64) -> std/i64"]
      rustBlock `shouldNotContain` [("  factorial : fun/public  (ghc-prim/Int#) -> ghc-prim/Int#" :: Text)]
    it "omits module headers that carry no symbols" $ do
      let ls = renderRepoMap [("Empty", "c", "[]", 0)] syms
      ls `shouldNotContain` [("Empty [c]" :: Text)]
  describe "organ_plan_stub" $ do
    it "names the tool in the organ_* namespace" $
      getFunctionName @OrganPlanStub `shouldBe` "organ_plan_stub"

    it "plans glue for an ingested crossing, fail-open to runtime checks on synthetic qnames" $
      withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        _ <- ingestPath idx tmp
        r <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "Factorial" "factorial" (Just "haskell") "factorial_rs" "factorial" (Just "rust") (Just "rs_island_fac") Nothing False)
        let out = organPlanStubOutput r
        -- Both fixtures use the synthetic std/int qname, which has no
        -- dictionary axiom: memberFor falls back to the dynamic member
        -- and the plan honestly demands runtime checks instead of
        -- pretending the crossing is proven.
        opsoVerdict out `shouldBe` "licensed-with-runtime-checks"
        T.unpack (opsoCaller out) `shouldContain` "omni_haskell_Factorial_factorial"
        T.unpack (opsoCallee out) `shouldContain` "omni_rust_factorial_rs_factorial"
        -- The explicit island-entry override flows into the filled
        -- trampoline (C2), addressed by its own name.
        T.unpack (opsoCallee out) `shouldContain` "rs_island_fac"
        opsoStubs out `shouldContain` ["// marshal notes:"]

    it "refuses a real unlicensed crossing (rust i64 result into ocaml's 63-bit int)" $
      withOcamlIndex $ \tmp -> do
        idx <- defaultOrganIndex
        _ <- ingestPath idx tmp
        r <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "factorial_oc" "factorial" (Just "ocaml") "factorial_rs" "factorial" (Just "rust") Nothing Nothing False)
        let out = organPlanStubOutput r
        -- The result flows callee→caller: rust std/i64 (64 bits) into
        -- OCaml's 63-bit tagged int is a genuine narrowing.
        opsoVerdict out `shouldBe` "unlicensed-narrowing"
        all ("//" `T.isPrefixOf`) (opsoStubs out) `shouldBe` True
        any ("axiom:" `T.isInfixOf`) (opsoStubs out) `shouldBe` True

    it "fails closed when a symbol is not in the index" $
      withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        _ <- ingestPath idx tmp
        r <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "Missing" "factorial" Nothing "factorial_rs" "factorial" (Just "rust") Nothing Nothing False)
        let out = organPlanStubOutput r
        opsoVerdict out `shouldBe` "unlicensed-resolve"
        any ("Symbol not in the index" `T.isInfixOf`) (opsoStubs out) `shouldBe` True

    it "emits the C4 ABI adapter for a haskell callee when requested" $
      withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        _ <- ingestPath idx tmp
        -- rust caller -> haskell callee (FacPure, empty effect row):
        -- the C4 projection bridges the wire's plain int64_t ABI onto
        -- the island's own (the compiler's capi header supplies StgInt).
        r <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "factorial_pure" "factorial" (Just "rust") "FacPure" "factorial" (Just "haskell") (Just "hs_island_factorial") (Just "hs_island_factorial") False)
        let out = organPlanStubOutput r
            adapterTxt = T.unpack (T.unlines (opsoAdapter out))
        opsoVerdict out `shouldBe` "licensed-with-runtime-checks"
        adapterTxt `shouldContain` "// ABI adapter: GHC island (generated, C4)"
        adapterTxt `shouldContain` "#include \"FacPure_api.h\""
        adapterTxt `shouldContain` "int64_t hs_island_factorial(int64_t n) {"
        -- The generated adapter targets the island's convention entry:
        -- Module_name, matching what the trampoline fills.
        adapterTxt `shouldContain` "FacPure_factorial((HsInt64) n)"
        -- The full stubs bundle carries the adapter section too.
        any ("ABI adapter" `T.isInfixOf`) (opsoStubs out) `shouldBe` True
        -- Without the request, no adapter: the default stays exactly as
        -- C2/C3 left it.
        r2 <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "factorial_pure" "factorial" (Just "rust") "FacPure" "factorial" (Just "haskell") (Just "hs_island_factorial") Nothing False)
        opsoAdapter (organPlanStubOutput r2) `shouldBe` []

    it "emits the C4 ABI adapter for a koka callee with the runtime contract" $
      withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        _ <- ingestPath idx tmp
        r <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "factorial_pure" "factorial" (Just "rust") "factorial" "island-factorial" (Just "koka") (Just "kk_island_factorial") (Just "kk_island_factorial") False)
        let out = organPlanStubOutput r
            adapterTxt = T.unpack (T.unlines (opsoAdapter out))
        opsoVerdict out `shouldBe` "licensed-with-runtime-checks"
        adapterTxt `shouldContain` "// ABI adapter: koka island (generated, C4)"
        adapterTxt `shouldContain` "#include \"factorial.h\""
        -- The runtime contract the hand-written adapter carried:
        adapterTxt `shouldContain` "kk_main_start(0, NULL)"
        adapterTxt `shouldContain` "kk_factorial__init(ctx)"
        adapterTxt `shouldContain` "kk_factorial__done(ctx)"
        -- The adapter entry is the trampoline's target; the projection
        -- calls koka's REAL export (kk_factorial_island_factorial --
        -- koka replaces '-' with '_'), never the adapter's own name.
        adapterTxt `shouldContain` "int64_t kk_island_factorial(int64_t n) {"
        adapterTxt `shouldContain` "kk_factorial_island_factorial(kk_integer_from_int64(n, ctx), ctx)"
        adapterTxt `shouldSatisfy` not . T.isInfixOf "kk_factorial_kk_island_factorial" . T.pack
        -- The koka callee-side wrapper documents the delegated contract.
        T.unpack (opsoCallee out) `shouldContain` "kk_main_start"

    it "emits the C5 bounded-contract shim for a declared-bound koka callee" $
      withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        _ <- ingestPath idx tmp
        -- the demo direction's REAL caller type: rust std/i64 (pure),
        -- dictionary-known, flowing into the bounded koka island.
        r <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "factorial_rs_i64" "factorial" (Just "rust") "factorial" "bounded-factorial" (Just "koka") (Just "kk_bounded_island") (Just "kk_bounded_island") True)
        let out = organPlanStubOutput r
            emapTxt = T.unpack (T.unlines (opsoEffectMap out))
        -- The declared bound converts the crossing to licensed-bounded.
        opsoVerdict out `shouldBe` "licensed-bounded"
        -- The shim is the checked form: exact koka-side guards on the
        -- arg and the result, wired through the C3 handle/try sentinel.
        emapTxt `shouldContain` "bcheck_arg_1"
        emapTxt `shouldContain` "bcheck_result"
        emapTxt `shouldContain` "1152921504606846976" -- 2^60 literal
        emapTxt `shouldContain` "bound violation"
        emapTxt `shouldContain` "handle/try("
        emapTxt `shouldContain` "min-int64"
        -- The trampoline targets the adapter's entry; the adapter (when
        -- requested) forwards to the mapped checked entry.
        T.unpack (opsoCallee out) `shouldContain` "kk_bounded_island"

    it "emits the C5 OCaml adapter with the pre-boxing bound check" $
      withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        _ <- ingestPath idx tmp
        -- rust std/i64 caller -> OCaml bounded61 island: the declared
        -- bound licenses the crossing that unbounded Stdlib/int would
        -- refuse as a narrowing, and the adapter must check BEFORE
        -- Val_long boxing (which would silently drop the top bits).
        r <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "factorial_rs_i64" "factorial" (Just "rust") "factorial_oc" "island-factorial" (Just "ocaml") (Just "ocaml_island_factorial") (Just "ocaml_island_factorial") False)
        let out = organPlanStubOutput r
            adapterTxt = T.unpack (T.unlines (opsoAdapter out))
        opsoVerdict out `shouldBe` "licensed-bounded"
        adapterTxt `shouldContain` "// ABI adapter: OCaml island (generated, C4)"
        -- Effect map: the island's exceptions surface as the wire's
        -- sentinel (caml_callback_exn + Is_exception_result), not a
        -- process-terminating caml_callback.
        adapterTxt `shouldContain` "caml_callback_exn"
        adapterTxt `shouldContain` "Is_exception_result(r)"
        -- The bound check guards before the box, and a violation
        -- returns the wire's status sentinel.
        adapterTxt `shouldContain` "2305843009213693952" -- 2^61 literal
        adapterTxt `shouldContain` "wire status sentinel"
        adapterTxt `shouldContain` "ocaml_island_factorial(int64_t n)"
        -- The RTS lifecycle entries follow the namespace convention.
        adapterTxt `shouldContain` "omni_oc_factorial_oc_island_init"
        adapterTxt `shouldContain` "caml_main"

    it "licenses the FBig big-big bounded crossing over the wire" $
      withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        _ <- ingestPath idx tmp
        -- koka's UNBOUNDED arbitrary-precision int calling a BOUNDED
        -- bigint island: before the FBig gate this was silently
        -- licensed-lossless (the boundGate exited for non-int pairs).
        -- The wire's int64 ABI is the carrier; the shim checks the
        -- bound in koka's exact big-int arithmetic.
        r <- runPrompt @StatelessConf [] [] "organ-test" defaultRequest $
          toolExec @OrganPlanStub @StatelessConf (OrganPlanStub "factorial_big" "big-factorial" (Just "koka") "factorial_big_bounded" "big-bounded-factorial" (Just "koka") (Just "kk_big_island") (Just "kk_big_island") True)
        let out = organPlanStubOutput r
            emapTxt = T.unpack (T.unlines (opsoEffectMap out))
        -- Not lossless: the declared bound converts the crossing to
        -- the checked form.
        opsoVerdict out `shouldBe` "licensed-bounded"
        -- The shim guards both corners in the callee's big-int domain:
        emapTxt `shouldContain` "bcheck_arg_1"
        emapTxt `shouldContain` "bcheck_result"
        emapTxt `shouldContain` "1152921504606846976" -- 2^60 literal
        emapTxt `shouldContain` "handle/try("
        emapTxt `shouldContain` "min-int64"
        opsoVerdict out `shouldNotBe` "licensed-lossless"

  where
    sameArgs ta tb =
      boundaryReport (OrganCheckBoundary "Factorial" "factorial" (Just "haskell") "factorial_rs" "factorial" (Just "rust")) (Just "haskell") ta (typeHeadline ta) (Just "rust") tb (typeHeadline tb)
    sameArgsNoLang ta tb =
      boundaryReport (OrganCheckBoundary "Factorial" "factorial" (Just "haskell") "factorial_rs" "factorial" (Just "rust")) Nothing ta (typeHeadline ta) Nothing tb (typeHeadline tb)
    -- fnTy above hardcodes module "std" for every qname, which cannot
    -- express the real dictionary qnames (Stdlib/int, std/i64) the
    -- axiom-table tests need.
    fnTyQ :: [(Text, Text)] -> Text -> (Text, Text) -> A.Value
    fnTyQ args eff (rmod, rname) =
      A.object
        [ "fn"
            A..= A.object
              [ "args" A..= [A.object ["multiplicity" A..= ("many" :: Text), "type" A..= qn a] | a <- args],
                "effect" A..= A.object ["effects" A..= [A.object ["module" A..= ("std" :: Text), "name" A..= A.object ["text" A..= eff]]]],
                "result" A..= qn (rmod, rname)
              ]
        ]
      where
        qn (m, n) = A.object ["con" A..= A.object ["qname" A..= A.object ["module" A..= m, "name" A..= A.object ["text" A..= n]]]]
    ocamlDoc :: A.Value
    ocamlDoc =
      A.object
        [ "schema_version" A..= ("1.0.0" :: Text),
          "metadata" A..= A.object ["source_language" A..= ("ocaml" :: Text), "shim_version" A..= ("0.1.0" :: Text)],
          "module"
            A..= A.object
              [ "name" A..= ("factorial_oc" :: Text),
                "definitions" A..= [def "factorial_oc" "factorial" "public" (fnTyQ [("Stdlib", "int")] "io" ("Stdlib", "int"))],
                "data_types" A..= ([] :: [A.Value]),
                "effect_decls" A..= ([] :: [A.Value])
              ]
        ]
    rustDoc64 :: A.Value
    rustDoc64 =
      A.object
        [ "schema_version" A..= ("1.0.0" :: Text),
          "metadata" A..= A.object ["source_language" A..= ("rust" :: Text), "shim_version" A..= ("0.1.0" :: Text)],
          "module"
            A..= A.object
              [ "name" A..= ("factorial_rs" :: Text),
                "definitions" A..= [def "factorial_rs" "factorial" "public" (fnTyQ [("std", "i64")] "io" ("std", "i64"))],
                "data_types" A..= ([] :: [A.Value]),
                "effect_decls" A..= ([] :: [A.Value])
              ]
        ]
    withOcamlIndex act = do
      tmpRoot <- getTemporaryDirectory
      (fp, h) <- openTempFile tmpRoot "organ-test"
      hClose h
      removeFile fp
      createDirectory fp
      A.encodeFile (fp ++ "/factorial_oc.json") ocamlDoc
      A.encodeFile (fp ++ "/factorial_rs.json") rustDoc64
      old <- lookupEnv "ORGAN_INDEX"
      setEnv "ORGAN_INDEX" (fp ++ "/index.db")
      r <- act fp
      case old of
        Just v -> setEnv "ORGAN_INDEX" v
        Nothing -> unsetEnv "ORGAN_INDEX"
      removeDirectoryRecursive fp
      return r
