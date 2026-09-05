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
import IntelliMonad.Types (HasFunctionObject (..))

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
              A..= [ def "factorial" "public" (fnTy ["int"] "pure" "int"),
                     def "main" "public" (fnTy [] "io" "unit")
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
              A..= [ def "factorial" "public" (fnTy ["int"] "io" "int")
                   ],
            "data_types" A..= ([] :: [A.Value]),
            "effect_decls" A..= ([] :: [A.Value])
          ]
    ]

-- | A definition with the given name/visibility and a fn type
-- (args ->{effects} result).
def :: Text -> Text -> A.Value -> A.Value
def n vis ty =
  A.object
    [ "name" A..= A.object ["module" A..= ("?" :: Text), "name" A..= A.object ["text" A..= n, "unique" A..= (1 :: Int)]],
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
withFreshIndex :: (FilePath -> IO a) -> IO a
withFreshIndex act = do
  tmpRoot <- getTemporaryDirectory
  bracket_ (pure ()) (pure ()) $ do
    withTempDir tmpRoot $ \tmp -> do
      let f1 = tmp ++ "/factorial.json"
          f2 = tmp ++ "/factorial_rs.json"
      A.encodeFile f1 sampleDoc
      A.encodeFile f2 rustDoc
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

spec :: Spec
spec = do
  describe "ingestPath" $ do
    it "ingests both documents and reports zero failures" $ do
      (ok, bad, errs) <- withFreshIndex $ \tmp -> do
        idx <- defaultOrganIndex
        ingestPath idx tmp
      ok `shouldBe` 2
      bad `shouldBe` 0
      errs `shouldBe` []

    it "records a diagnostic for malformed JSON" $ do
      (ok, bad, _errs) <- withFreshIndex $ \tmp -> do
        writeFile (tmp ++ "/broken.json") "{not json"
        idx <- defaultOrganIndex
        ingestPath idx tmp
      ok `shouldBe` 2
      bad `shouldBe` 1

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

    it "classifies identical types" $
      brVerdict (sameArgs (fnTy ["int"] "pure" "int") (fnTy ["int"] "pure" "int")) `shouldBe` "identical"

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
  where
    sameArgs ta tb =
      boundaryReport (OrganCheckBoundary "Factorial" "factorial" (Just "haskell") "factorial_rs" "factorial" (Just "rust")) ta (typeHeadline ta) tb (typeHeadline tb)
