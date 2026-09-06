{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tools over an organ-bank OrganIR index — the LLM observability
-- sidecar for the polyglot compiler stack.
--
-- Organ-bank's 29 shims each extract a language's compiler IR and emit
-- OrganIR JSON (see @organ-bank/doc/writing-a-shim.md@). This module is
-- the read-only consumer of those artifacts: it ingests one JSON
-- document per module into a standalone SQLite index (WAL, same
-- low-level @Database.Sqlite@ binding the session store uses), then
-- serves five tools:
--
--   * 'organ_repo_map'         — token-budgeted skeleton of every module
--   * 'organ_find_symbol'      — defs + refs for a name, across languages
--   * 'organ_check_boundary'   — compare two QNames' type + effect rows
--   * 'organ_diagnostics'      — what the shims reported during extraction
--   * 'organ_ingest'           — (re)index OrganIR JSON files or directories
--
-- The index file location comes from the @ORGAN_INDEX@ environment
-- variable (default @~\/.organ\/organ-index.db@). Ingestion input
-- defaults to the @ORGAN_BANK_DIR@ environment variable.
--
-- Soundness note: boundary reports are descriptive by default — they
-- compare the two sides' 'Ty' and 'EffectRow' JSON and classify
-- differences in prose. When both sides' languages have entries in the
-- representation dictionary ("IntelliMonad.Tools.OrganBank.Dictionary")
-- the verdict is strengthened: every argument and the (direction-aware)
-- result are checked against the per-language axioms, and a crossing
-- the axioms cannot license is reported @unlicensed-boundary@. That is
-- still not a cross-language typechecker — it is an auditable axiom
-- table plus fail-closed aggregation (see doc/organ-tool-plan.md,
-- Phase C).
module IntelliMonad.Tools.OrganBank
  ( OrganRepoMap (..)
  , OrganFindSymbol (..)
  , OrganCheckBoundary (..)
  , OrganPlanStub (..)
  , OrganPlanStubOutput (..)
  , organPlanStubOutput
  , OrganDiagnostics (..)
  , OrganIngest (..)
  , defaultOrganIndex
  , ingestPath
  , typeHeadline
  , boundaryReport
  , renderRepoMap
  , BoundaryReport (..)
  -- * Internal (exposed for tests)
  , withIndex
  , queryRows
  , pvText
  , DiagRow (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Vector as V
import Database.Persist.Sqlite (PersistValue (..))
import Database.Sqlite
import GHC.Generics (Generic)
import IntelliMonad.Types
import qualified IntelliMonad.Tools.OrganBank.Dictionary as D
import qualified IntelliMonad.Tools.OrganBank.Stubs as S
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))

--------------------------------------------------------------------------------
-- Schema (JSON side, exactly what organ-ir emits)
--------------------------------------------------------------------------------

-- | organ-ir emits snake_case keys; our records carry a short type
-- prefix (od, m, q, n, d). This maps one to the other.
organOptions :: String -> A.Options
organOptions prefix =
  A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop (length prefix)}

-- | @name@ object: text + unique (unique optional in the wild).
data NameJ = NameJ
  { nText :: Text,
    nUnique :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON NameJ where parseJSON = A.genericParseJSON (organOptions "n")
instance A.ToJSON NameJ where toJSON = A.genericToJSON (organOptions "n")

-- | @qname@: module + name.
data QNameJ = QNameJ
  { qModule :: Text,
    qName :: NameJ
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON QNameJ where parseJSON = A.genericParseJSON (organOptions "q")
instance A.ToJSON QNameJ where toJSON = A.genericToJSON (organOptions "q")

-- | Metadata: source_language and shim_version are required by the schema.
data MetadataJ = MetadataJ
  { mSourceLanguage :: Text,
    mShimVersion :: Text,
    mCompilerVersion :: Maybe Text,
    mSourceFile :: Maybe Text,
    mTimestamp :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON MetadataJ where parseJSON = A.genericParseJSON (organOptions "m")
instance A.ToJSON MetadataJ where toJSON = A.genericToJSON (organOptions "m")

-- | One @definition@. @type@, @expr@, @sort@, @visibility@ are required;
-- we keep type/expr verbatim as JSON for lossless round-tripping into
-- the index and derive a compact headline for display.
data DefinitionJ = DefinitionJ
  { dName :: QNameJ,
    dType :: A.Value,
    dExpr :: A.Value,
    dSort :: Text,
    dVisibility :: Text
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON DefinitionJ where parseJSON = A.genericParseJSON (organOptions "d")
instance A.ToJSON DefinitionJ where toJSON = A.genericToJSON (organOptions "d")

-- | One OrganIR document: schema_version, metadata, module.
data OrganDoc = OrganDoc
  { odSchemaVersion :: Text,
    odMetadata :: MetadataJ,
    odModule :: ModuleJ
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON OrganDoc where parseJSON = A.genericParseJSON (organOptions "od")
instance A.ToJSON OrganDoc where toJSON = A.genericToJSON (organOptions "od")

data ModuleJ = ModuleJ
  { mName :: Text,
    mExports :: Maybe [Text],
    mImports :: Maybe [A.Value],
    mDefinitions :: [DefinitionJ],
    mDataTypes :: Maybe [A.Value],
    mEffectDecls :: Maybe [A.Value]
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON ModuleJ where parseJSON = A.genericParseJSON (organOptions "m")
instance A.ToJSON ModuleJ where toJSON = A.genericToJSON (organOptions "m")

--------------------------------------------------------------------------------
-- SQLite index: schema + low-level helpers
--------------------------------------------------------------------------------

indexSchema :: [Text]
indexSchema =
  [ "CREATE TABLE IF NOT EXISTS symbols (\
    \  qname TEXT NOT NULL,\
    \  module TEXT NOT NULL,\
    \  name TEXT NOT NULL,\
    \  sort TEXT NOT NULL,\
    \  visibility TEXT NOT NULL,\
    \  lang TEXT NOT NULL,\
    \  source_file TEXT,\
    \  ty_json TEXT NOT NULL,\
    \  expr_json TEXT NOT NULL,\
    \  type_headline TEXT NOT NULL,\
    \  PRIMARY KEY (lang, qname)\
    \)",
    "CREATE INDEX IF NOT EXISTS idx_symbols_name ON symbols(name)",
    "CREATE TABLE IF NOT EXISTS refs (\
    \  name TEXT NOT NULL,\
    \  ref_module TEXT NOT NULL,\
    \  lang TEXT NOT NULL,\
    \  kind TEXT NOT NULL\
    \)",
    "CREATE INDEX IF NOT EXISTS idx_refs_name ON refs(name)",
    "CREATE TABLE IF NOT EXISTS diagnostics (\
    \  source_file TEXT,\
    \  severity TEXT NOT NULL,\
    \  message TEXT NOT NULL,\
    \  ingested_at TEXT NOT NULL\
    \)",
    "CREATE TABLE IF NOT EXISTS modules (\
    \  module TEXT NOT NULL,\
    \  lang TEXT NOT NULL,\
    \  exports TEXT NOT NULL,\
    \  n_defs INTEGER NOT NULL,\
    \  PRIMARY KEY (lang, module)\
    \)"
  ]

-- | Default index location: @$ORGAN_INDEX@ or @~/.organ/organ-index.db@.
defaultOrganIndex :: IO FilePath
defaultOrganIndex =
  lookupEnv "ORGAN_INDEX" >>= \case
    Just p -> return p
    Nothing -> do
      home <- getHomeDirectory
      return (home </> ".organ" </> "organ-index.db")

-- | Run a write action against the index with a fresh connection.
withIndex :: forall a. FilePath -> (Connection -> IO a) -> IO a
withIndex path act = do
  let dir = reverse . dropWhile (/= '/') . reverse $ path
  unless' (null dir) (createDirectoryIfMissing True dir)
  conn <- open (T.pack path)
  r <- try (do
    forM_ indexSchema $ \ddl -> do
      stmt <- prepare conn ddl
      _ <- stepConn conn stmt
      finalize stmt
    act conn) :: IO (Either SomeException a)
  close conn
  either (\e -> ioError (userError ("organ index: " <> show e))) return r
  where
    unless' cond act' = if cond then return () else act'

-- | JSON value → compact JSON Text (exactly the bytes organ-ir emits).
jText :: A.Value -> Text
jText = TEnc.decodeUtf8 . BL.toStrict . A.encode

-- | Execute a parameterized statement, ignoring results (DML/DDL).
execSql :: Connection -> Text -> [PersistValue] -> IO ()
execSql conn sql params = do
  stmt <- prepare conn sql
  bind stmt params
  _ <- stepConn conn stmt
  finalize stmt

-- | Run a SELECT, mapping each row's columns through the given function.
queryRows :: Connection -> Text -> [PersistValue] -> ([PersistValue] -> IO a) -> IO [a]
queryRows conn sql params rowFn = do
  stmt <- prepare conn sql
  bind stmt params
  let loop acc = do
        r <- stepConn conn stmt
        case r of
          Done -> return (reverse acc)
          Row -> do
            cols <- columns stmt
            row <- rowFn cols
            loop (row : acc)
  rows <- loop []
  finalize stmt
  return rows

pvText :: PersistValue -> Text
pvText (PersistText t) = t
pvText v = T.pack (show v)

pvMaybeText :: PersistValue -> Maybe Text
pvMaybeText PersistNull = Nothing
pvMaybeText (PersistText t) = Just t
pvMaybeText v = Just (pvText v)

pvInt :: PersistValue -> Int
pvInt (PersistInt64 i) = fromIntegral i
pvInt _ = 0

-- | Bind an optional filter argument: Nothing becomes SQL NULL, which
-- the @(? IS NULL OR col = ?)@ query pattern treats as "no filter".
sqlMaybeText :: Maybe Text -> PersistValue
sqlMaybeText = maybe PersistNull PersistText

-- | Render the compact repo-map lines: one header per (lang, module)
-- with each definition's signature headline indented under it. A module
-- is identified by (lang, module): the same module name can exist in
-- every language, and each carries its own symbol set. This is a pure
-- function so the grouping discipline is unit-testable.
renderRepoMap ::
  [(Text, Text, Text, Int)] ->
  -- ^ (module, lang, exports, n_defs) rows from the modules table
  [(Text, Text, Text, Text, Text, Text)] ->
  -- ^ (module, lang, name, sort, visibility, type_headline) rows
  [Text]
renderRepoMap mods syms =
  let byMod = Map.fromListWith (++) [((l, m), [(n, s, v, h)]) | (m, l, n, s, v, h) <- syms]
      fmtModule (m, lang, _exports, _n) =
        let defs = Map.findWithDefault [] (lang, m) byMod
            head' = m <> " [" <> lang <> "]"
            body = ["  " <> n <> " : " <> s <> "/" <> v <> "  " <> h | (n, s, v, h) <- defs]
         in if null defs then [] else head' : body
   in concatMap fmtModule mods

--------------------------------------------------------------------------------
-- Ingestion
--------------------------------------------------------------------------------

-- | Ingest one OrganIR JSON file. Returns the module name on success;
-- parse errors AND storage exceptions come back as 'Left' so one bad
-- file never aborts a batch.
ingestFile :: Connection -> FilePath -> IO (Either Text Text)
ingestFile conn fp = do
  r <- try (A.eitherDecodeFileStrict' fp) :: IO (Either SomeException (Either String OrganDoc))
  case r of
    Left e -> return (Left (T.pack (show e)))
    Right (Left err) -> return (Left (T.pack err))
    Right (Right doc) ->
      try (upsertModule conn doc >> return (Right (mName (odModule doc)))) >>= \case
        Left (e :: SomeException) -> return (Left (T.pack ("stored parse but upsert failed: " <> show e)))
        Right res -> return res

--------------------------------------------------------------------------------
-- Diagnostics-envelope ingestion (organ-extract --diag)
--------------------------------------------------------------------------------

-- | The per-file rows of organ-extract's @--diag@ envelope
-- (organ_diag_version 1). Only the fields the index stores are decoded;
-- warnings/shim are optional and tolerated-absent so older envelopes
-- still ingest.
data DiagRow = DiagRow
  { drPath :: FilePath,
    drSeverity :: Text,
    drMessage :: Text
  }
  deriving (Eq, Show)

instance A.FromJSON DiagRow where
  parseJSON = A.withObject "DiagRow" $ \o -> do
    path <- o A..: "path"
    status <- o A..: "status"
    stderrMsg <- o A..:? "stderr"
    let sev = if status == ("ok" :: Text) then "ok" else "error"
        msg = case (status, stderrMsg) of
          ("ok", Just e) | not (T.null e) -> "warning: " <> e
          ("ok", _) -> "ok"
          (_, Just e) | not (T.null e) -> e
          _ -> "failed (no stderr captured)"
    pure (DiagRow path sev msg)

-- | The organ-extract @--diag@ envelope (organ_diag_version 1):
-- @{"organ_diag_version": "1", "files": [...]}@. The version key is
-- tolerated-extra; only @files@ is decoded.
newtype DiagEnvelope = DiagEnvelope {deFiles :: [DiagRow]}
  deriving (Eq, Show)

instance A.FromJSON DiagEnvelope where
  parseJSON = A.withObject "DiagEnvelope" $ \o ->
    DiagEnvelope <$> o A..: "files"

-- | Ingest an organ-extract @--diag@ envelope: one diagnostics row per
-- file, keeping verbatim stderr so compiler messages survive the trip.
-- Files that extracted fine record a plain "ok" row; successes with
-- non-empty stderr record a warning.
ingestDiagEnvelope :: Connection -> FilePath -> IO (Either Text Text)
ingestDiagEnvelope conn fp = do
  r <- try (A.eitherDecodeFileStrict' fp) :: IO (Either SomeException (Either String DiagEnvelope))
  case r of
    Left e -> return (Left (T.pack (show e)))
    Right (Left err) -> return (Left (T.pack ("not a --diag envelope: " ++ err)))
    Right (Right env) ->
      try (do
        forM_ (deFiles env) $ \row ->
          execSql conn
            "INSERT INTO diagnostics(source_file, severity, message, ingested_at) VALUES (?, ?, ?, datetime('now'))"
            [PersistText (T.pack (drPath row)), PersistText (drSeverity row), PersistText (drMessage row)]
        return (Right (T.pack (show (length (deFiles env))) <> " diagnostic rows")))
        >>= \case
          Left (e :: SomeException) -> return (Left (T.pack ("diag rows rejected: " <> show e)))
          Right res -> return res

upsertModule :: Connection -> OrganDoc -> IO ()
upsertModule conn doc = do
  let mod' = odModule doc
      lang = mSourceLanguage (odMetadata doc)
      exports = T.intercalate "," (fromMaybe [] (mExports mod'))
  execSql conn
    "INSERT OR REPLACE INTO modules(module, lang, exports, n_defs) VALUES (?,?,?,?)"
    [PersistText (mName mod'), PersistText lang, PersistText exports, PersistInt64 (fromIntegral (length (mDefinitions mod')))]
  forM_ (mDefinitions mod') $ \d -> do
    let qn = qModule (dName d) <> "/" <> nText (qName (dName d))
    execSql conn
      "INSERT OR REPLACE INTO symbols(qname, module, name, sort, visibility, lang, source_file, ty_json, expr_json, type_headline) VALUES (?,?,?,?,?,?,?,?,?,?)"
      [ PersistText qn,
        PersistText (qModule (dName d)),
        PersistText (nText (qName (dName d))),
        PersistText (dSort d),
        PersistText (dVisibility d),
        PersistText lang,
        maybe PersistNull PersistText (mSourceFile (odMetadata doc)),
        PersistText (jText (dType d)),
        PersistText (jText (dExpr d)),
        PersistText (typeHeadline (dType d))
      ]
  forM_ (fromMaybe [] (mImports mod')) $ \imp ->
    case imp of
      A.String s ->
        execSql conn "INSERT OR REPLACE INTO refs(name, ref_module, lang, kind) VALUES (?,?,?,'import')"
        [PersistText s, PersistText (mName mod'), PersistText lang]
      A.Object o -> do
        let nm = qnameToText (A.Object o)
        execSql conn "INSERT OR REPLACE INTO refs(name, ref_module, lang, kind) VALUES (?,?,?,'import')"
          [PersistText nm, PersistText (mName mod'), PersistText lang]
      _ -> return ()

-- | Flatten a qname-shaped JSON value to \"module/name\" (best effort).
-- Accepts the bare @{module, name}@ shape and the tagged shapes the
-- OrganIR type/expr unions use (@{con: {qname: ...}}@ etc.) by
-- unwrapping a @qname@ member when present.
qnameToText :: A.Value -> Text
qnameToText (A.Object o)
  | Just inner <- KM.lookup "qname" o = qnameToText inner
  | otherwise =
      let m = KM.lookup "module" o
          n = KM.lookup "name" o
      in maybe "" asText m <> "/" <> maybe "" nameText n
  where
    asText (A.String s) = s
    asText v = T.take 80 (T.pack (show v))
    -- The name member may itself be an object ({text, unique}).
    nameText (A.Object no) = maybe "" asText (KM.lookup "text" no)
    nameText v = asText v
qnameToText _ = ""

-- | Head name of a type-shaped JSON value (the con/var tags of the
-- OrganIR type union): renders the inner qname, or a placeholder.
tyHeadName :: A.Value -> Text
tyHeadName v = case v of
  A.Object o
    | Just q <- KM.lookup "con" o -> qnameToText q
    | Just q <- KM.lookup "var" o -> qnameToText q
    | Just q <- KM.lookup "app" o -> qnameToText q
  _ -> "?"

-- | EffectRow-shaped JSON (@{effects: [qname...], tail}@ or a bare
-- array) rendered as comma-joined effect names. Elements are bare
-- qname objects per the schema ("std/core/div"), not tagged types.
effectRowText :: A.Value -> Text
effectRowText v = case v of
  A.Object o -> case KM.lookup "effects" o of
    Just (A.Array es) -> T.intercalate "," (map qnameToText (V.toList es))
    _ -> ""
  A.Array es -> T.intercalate "," (map qnameToText (V.toList es))
  _ -> ""

-- | A compact one-line signature for display in repo maps: for
-- function types \"(arg, arg) -> effect ret\", otherwise the tag.
typeHeadline :: A.Value -> Text
typeHeadline v = case v of
  A.Object o | Just fn <- KM.lookup "fn" o -> fnHeadline fn
  A.Object o | Just fv <- KM.lookup "forall" o ->
    case fv of
      A.Object b | Just body <- KM.lookup "body" b -> "forall. " <> typeHeadline body
      _ -> "forall"
  A.Object o | Just tv <- KM.lookup "con" o -> qnameToText tv
  A.Object o | Just tv <- KM.lookup "var" o -> qnameToText tv
  A.Object o | Just tv <- KM.lookup "app" o -> "(" <> qnameToText tv <> ")"
  A.Object o -> T.intercalate "|" (map Key.toText (KM.keys o))
  _ -> "other"
  where
    fnHeadline fn = case fn of
      A.Object o ->
        let args = case KM.lookup "args" o of
              Just (A.Array as) ->
                [ maybe "?" typeHeadline (argTy a) | a <- V.toList as ]
              _ -> []
            eff = maybe "" effectRowText (KM.lookup "effect" o)
            ret = maybe "?" typeHeadline (KM.lookup "result" o)
            effTag = if T.null eff then "" else "{" <> eff <> "} "
        in "(" <> T.intercalate ", " args <> ") -> " <> effTag <> ret
      _ -> "fn"
    argTy a = case a of
      A.Object ao -> KM.lookup "type" ao
      _ -> Nothing

--------------------------------------------------------------------------------
-- Ingestion driver: files or directories
--------------------------------------------------------------------------------

organJsonFiles :: FilePath -> IO [FilePath]
organJsonFiles p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      entries <- listDirectory p
      sub <- forM (sort entries) $ \e -> do
        let full = p </> e
        d <- doesDirectoryExist full
        if d then organJsonFiles full else return [full | isJson e]
      return (concat sub)
    else return [p | isJson p]
  where
    isJson e = takeExtension e == ".json"

-- | Ingest a file or directory tree of OrganIR JSON into the index.
-- Returns (ingested, failed) counts plus per-file error messages.
-- A file that carries the organ-extract @--diag@ envelope
-- (@"organ_diag_version"@ key) is routed to the diagnostics ingester
-- instead of the module ingester, so one tool covers both artifact
-- kinds; the envelope is self-describing so the sniff is reliable.
ingestPath :: FilePath -> FilePath -> IO (Int, Int, [Text])
ingestPath indexPath inputPath = do
  files <- organJsonFiles inputPath
  withIndex indexPath $ \conn -> do
    results <- forM files $ \fp -> sniffAndIngest conn fp
    let errs = [(f, e) | (f, Left e) <- zip files results]
    forM_ errs $ \(f, e) ->
      execSql conn "INSERT INTO diagnostics(source_file, severity, message, ingested_at) VALUES (?, 'error', ?, datetime('now'))"
        [PersistText (T.pack f), PersistText e]
    let ok = length [() | Right _ <- results]
        bad = length errs
    return (ok, bad, [T.pack f <> ": " <> e | (f, e) <- errs])

-- | Route one JSON file to the right ingester by content: diag
-- envelopes (self-describing via @organ_diag_version@) go to the
-- diagnostics path, everything else is treated as an OrganIR module.
sniffAndIngest :: Connection -> FilePath -> IO (Either Text Text)
sniffAndIngest conn fp = do
  isDiag <- looksLikeDiagEnvelope fp
  if isDiag then ingestDiagEnvelope conn fp else ingestFile conn fp

looksLikeDiagEnvelope :: FilePath -> IO Bool
looksLikeDiagEnvelope fp = do
  r <- try (A.eitherDecodeFileStrict' fp) :: IO (Either SomeException (Either String A.Value))
  case r of
    Right (Right (A.Object o)) -> return (KM.member "organ_diag_version" o)
    _ -> return False

--------------------------------------------------------------------------------
-- Tool inputs/outputs
--------------------------------------------------------------------------------

-- | Input for 'organ_ingest'.
data OrganIngest = OrganIngest
  { oiPath :: Text
  }
  deriving (Eq, Show, Generic, JSONSchema, A.FromJSON, A.ToJSON)

instance HasFunctionObject OrganIngest where
  getFunctionName = "organ_ingest"
  getFunctionDescription = "Index OrganIR JSON files or a directory tree from organ-bank shims into the organ index. Returns ingested/failed counts and any per-file errors."
  getFieldDescription "oiPath" = "Path to an OrganIR JSON file or a directory containing them"
  getFieldDescription _ = ""

data OrganIngestOutput = OrganIngestOutput
  { oiIngested :: Int,
    oiFailed :: Int,
    oiErrors :: [Text],
    oiIndexPath :: Text
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

instance Tool OrganIngest where
  data Output OrganIngest = OrganIngestOut OrganIngestOutput
    deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

  toolExec args = do
    idx <- liftIO defaultOrganIndex
    (ok, bad, errs) <- liftIO (ingestPath idx (T.unpack args.oiPath))
    return $ OrganIngestOut (OrganIngestOutput ok bad errs (T.pack idx))

-- | Input for 'organ_repo_map'.
data OrganRepoMap = OrganRepoMap
  { ormLang :: Maybe Text,
    ormModule :: Maybe Text
  }
  deriving (Eq, Show, Generic, JSONSchema, A.FromJSON, A.ToJSON)

instance HasFunctionObject OrganRepoMap where
  getFunctionName = "organ_repo_map"
  getFunctionDescription = "Compact structural map of indexed polyglot modules: module, language, and each definition's sort/visibility/signature headline — no bodies. Cheapest way to see what exists before reading files."
  getFieldDescription "ormLang" = "Optional language filter (e.g. haskell, mercury, rust)"
  getFieldDescription "ormModule" = "Optional module name filter (exact match)"
  getFieldDescription _ = ""

data OrganRepoMapOutput = OrganRepoMapOutput
  { ormLines :: [Text],
    ormSymbols :: Int,
    ormModules :: Int
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

instance Tool OrganRepoMap where
  data Output OrganRepoMap = OrganRepoMapOut OrganRepoMapOutput
    deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

  toolExec args = do
    idx <- liftIO defaultOrganIndex
    out <- liftIO $ withIndex idx $ \conn -> do
      mods <-
        queryRows
          conn
          "SELECT module, lang, exports, n_defs FROM modules WHERE (? IS NULL OR lang = ?) AND (? IS NULL OR module = ?) ORDER BY module, lang"
          [sqlMaybeText args.ormLang, sqlMaybeText args.ormLang, sqlMaybeText args.ormModule, sqlMaybeText args.ormModule]
          (\r -> return (pvText (r !! 0), pvText (r !! 1), pvText (r !! 2), pvInt (r !! 3)))
      syms <-
        queryRows
          conn
          "SELECT module, lang, name, sort, visibility, type_headline FROM symbols WHERE (? IS NULL OR lang = ?) AND (? IS NULL OR module = ?) ORDER BY module, lang, name"
          [sqlMaybeText args.ormLang, sqlMaybeText args.ormLang, sqlMaybeText args.ormModule, sqlMaybeText args.ormModule]
          (\r -> return (pvText (r !! 0), pvText (r !! 1), pvText (r !! 2), pvText (r !! 3), pvText (r !! 4), pvText (r !! 5)))
      return (mods, syms)
    let (mods, syms) = out
        lines' = renderRepoMap mods syms
    return $ OrganRepoMapOut (OrganRepoMapOutput lines' (length syms) (length mods))

-- | Input for 'organ_find_symbol'.
data OrganFindSymbol = OrganFindSymbol
  { ofsName :: Text
  }
  deriving (Eq, Show, Generic, JSONSchema, A.FromJSON, A.ToJSON)

instance HasFunctionObject OrganFindSymbol where
  getFunctionName = "organ_find_symbol"
  getFunctionDescription = "Find definitions and references of a symbol across every indexed language — the cross-boundary lookup grep cannot do safely across 26 languages at once."
  getFieldDescription "ofsName" = "Symbol name (unqualified), e.g. \"factorial\""
  getFieldDescription _ = ""

data OrganFindSymbolOutput = OrganFindSymbolOutput
  { ofDefs :: [Text],
    ofRefs :: [Text]
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

instance Tool OrganFindSymbol where
  data Output OrganFindSymbol = OrganFindSymbolOut OrganFindSymbolOutput
    deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

  toolExec args = do
    idx <- liftIO defaultOrganIndex
    (defs, refs) <- liftIO $ withIndex idx $ \conn -> do
      ds <- queryRows conn
        "SELECT module, name, sort, visibility, lang, source_file, type_headline FROM symbols WHERE name = ? ORDER BY lang, module"
        [PersistText args.ofsName]
        (\r -> return (pvText (r !! 0), pvText (r !! 1), pvText (r !! 2), pvText (r !! 3), pvText (r !! 4), pvMaybeText (r !! 5), pvText (r !! 6)))
      rs <- queryRows conn
        "SELECT ref_module, lang, kind FROM refs WHERE name = ? ORDER BY lang, ref_module"
        [PersistText args.ofsName]
        (\r -> return (pvText (r !! 0), pvText (r !! 1), pvText (r !! 2)))
      return (ds, rs)
    let fmtDef (m, n, s, v, lang, sf, h) =
          n <> " : " <> s <> "/" <> v <> "  " <> h <> "  [" <> lang <> " " <> m <> maybe "" (\f -> " @" <> f) sf <> "]"
        fmtRef (m, lang, k) = k <> " in " <> m <> " [" <> lang <> "]"
    return $ OrganFindSymbolOut (OrganFindSymbolOutput (map fmtDef defs) (map fmtRef refs))

-- | Input for 'organ_check_boundary'. The language fields are
-- optional disambiguators: the same module+name routinely exists in
-- many languages once several shims' outputs are indexed.
data OrganCheckBoundary = OrganCheckBoundary
  { ocbModuleA :: Text,
    ocbNameA :: Text,
    ocbLangA :: Maybe Text,
    ocbModuleB :: Text,
    ocbNameB :: Text,
    ocbLangB :: Maybe Text
  }
  deriving (Eq, Show, Generic, JSONSchema, A.FromJSON, A.ToJSON)

instance HasFunctionObject OrganCheckBoundary where
  getFunctionName = "organ_check_boundary"
  getFunctionDescription = "Descriptively compare two definitions' types and effect rows across language boundaries. Reports arity, type, effect and multiplicity differences in prose. Descriptive only — not a sound cross-language typechecker."
  getFieldDescription "ocbModuleA" = "Module of the caller side"
  getFieldDescription "ocbNameA" = "Symbol name of the caller side"
  getFieldDescription "ocbLangA" = "Optional language of the caller side when the name exists in several (e.g. haskell)"
  getFieldDescription "ocbModuleB" = "Module of the callee side"
  getFieldDescription "ocbNameB" = "Symbol name of the callee side"
  getFieldDescription "ocbLangB" = "Optional language of the callee side"
  getFieldDescription _ = ""

data BoundaryReport = BoundaryReport
  { brA :: Text,
    brB :: Text,
    brArity :: Maybe (Int, Int),
    brEffectDiff :: Maybe (Text, Text),
    brVerdict :: Text,
    brDetail :: [Text]
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

data OrganCheckBoundaryOutput = OrganCheckBoundaryOutput
  { ocbReport :: BoundaryReport
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

instance Tool OrganCheckBoundary where
  data Output OrganCheckBoundary = OrganCheckBoundaryOut OrganCheckBoundaryOutput
    deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

  toolExec args = do
    idx <- liftIO defaultOrganIndex
    mreport <- liftIO $ withIndex idx $ \conn -> do
      a <- fetchOne conn (args.ocbModuleA) (args.ocbNameA) (args.ocbLangA)
      b <- fetchOne conn (args.ocbModuleB) (args.ocbNameB) (args.ocbLangB)
      return ((,) <$> a <*> b)
    case mreport of
      Left problem ->
        return $ OrganCheckBoundaryOut
          (OrganCheckBoundaryOutput (BoundaryReport "" "" Nothing Nothing "not resolvable" [problem]))
      Right ((ta, ea, la), (tb, eb, lb)) ->
        return $ OrganCheckBoundaryOut (OrganCheckBoundaryOutput (boundaryReport args (Just la) ta ea (Just lb) tb eb))
    where
      fetchOne conn m n mlang = do
        let sql = case mlang of
              Just _ -> "SELECT ty_json, type_headline, lang FROM symbols WHERE module = ? AND name = ? AND lang = ?"
              Nothing -> "SELECT ty_json, type_headline, lang FROM symbols WHERE module = ? AND name = ?"
            params = [PersistText m, PersistText n] <> maybe [] ((: []) . PersistText) mlang
        rows <- queryRows conn sql params
          (\r -> return (pvText (r !! 0), pvText (r !! 1), pvText (r !! 2)))
        return $ case rows of
          [] -> Left "Symbol not in the index; run organ_ingest first or check spelling."
          [(tj, h, l)] ->
            case A.decode (BL.fromStrict (TEnc.encodeUtf8 tj)) of
              Just v -> Right (v, h, l)
              Nothing -> Left "Stored type JSON failed to re-decode (index corruption?)."
          multiple ->
            Left
              ( "Ambiguous: symbol exists in "
                  <> T.pack (show (length multiple))
                  <> " languages ("
                  <> T.intercalate ", " [l | (_, _, l) <- multiple]
                  <> "). Re-run with the lang qualifier."
              )

-- | Compare two verbatim type JSONs and classify the differences.
-- The @Maybe Text@ language arguments license the dictionary layer:
-- when both languages are known (and both sides are function types),
-- every argument and the result are checked against the representation
-- dictionary ('IntelliMonad.Tools.OrganBank.Dictionary') and the
-- verdict is strengthened — or to @unlicensed-boundary@ when an axiom
-- refutes the crossing. Positions without axioms fail closed.
--
-- Direction matters: argument values flow caller→callee, so each
-- argument position licenses /caller into callee/; the result value
-- flows callee→caller, so the result position licenses /callee into
-- caller/. A C @int32@ ↔ Rust @i64@ pair is licensed-widening on the
-- argument and unlicensed-narrowing on the return — the weakest link
-- wins.
boundaryReport ::
  OrganCheckBoundary ->
  Maybe Text ->
  A.Value ->
  Text ->
  Maybe Text ->
  A.Value ->
  Text ->
  BoundaryReport
boundaryReport args mlangA ta ha mlangB tb hb =
  let fnA = fnOf ta
      fnB = fnOf tb
      arity (Just (A.Object o)) = case KM.lookup "args" o of
        Just (A.Array as) -> Just (length (arrToList as))
        _ -> Just 0
      arity _ = Nothing
      arA = arity fnA
      arB = arity fnB
      effA = effectNames fnA
      effB = effectNames fnB
      effDiff = if effA == effB then Nothing else Just (effA, effB)
      bothFn = maybe False (const True) fnA && maybe False (const True) fnB
      argsOf mfn = case mfn of
        Just (A.Object o) -> case KM.lookup "args" o of
          Just (A.Array as) -> [t | A.Object a <- arrToList as, Just t <- [KM.lookup "type" a]]
          _ -> []
        _ -> []
      resOf mfn = case mfn of
        Just (A.Object o) -> KM.lookup "result" o
        _ -> Nothing
      -- tyHeadName unwraps the OrganIR tagged shapes ({con: {qname:
      -- ...}}, {var: ...}, {app: ...}); qnameToText alone would see only
      -- the outer tag and render "/".
      memberFor lang t =
        let r = tyHeadName t
            (m, n) = T.breakOnEnd "/" r
        in if T.null r || T.null m then Nothing else D.memberOf lang (T.dropEnd 1 m) n
      unknownSide mlang t mm =
        mlang <> ":" <> tyHeadName t
          <> (case mm of Just _ -> " (known)"; Nothing -> " (outside the dictionary)")
      licenseDir mlangFrom tFrom mlangTo tTo = case (mlangFrom, mlangTo) of
        (Just lf, Just lt) -> case (memberFor lf tFrom, memberFor lt tTo) of
          (Just mf, Just mt) -> D.license mf mt
          _ ->
            ( "unlicensed-unproven",
              ["no axiom for " <> unknownSide lf tFrom (memberFor lf tFrom) <> " vs " <> unknownSide lt tTo (memberFor lt tTo)]
            )
        _ -> ("unlicensed-unproven", ["language of one side unknown; the dictionary was not consulted"])
      posReports =
        [ ("arg " <> T.pack (show i), v, ax)
        | (i, (x, y)) <- zip [1 :: Int ..] (zip (argsOf fnA) (argsOf fnB)),
          (v, ax) <- [licenseDir mlangA x mlangB y]
        ]
      resReports = case (resOf fnA, resOf fnB) of
        -- The return value flows callee → caller.
        (Just x, Just y) -> [("result", v, ax) | (v, ax) <- [licenseDir mlangB y mlangA x]]
        _ -> []
      -- The dictionary judges a genuine crossing only: different,
      -- known languages, both function-shaped. A same-language
      -- comparison has no representation boundary, and non-function
      -- shapes have no positions to license.
      crossLang = bothFn && isJust mlangA && isJust mlangB && mlangA /= mlangB
      posAll = if crossLang then posReports <> resReports else []
      licVerdict = if crossLang then D.aggregate [v | (_, v, _) <- posAll] else Nothing
      overallLine v
        | v == "licensed-lossless" = "every value provably round-trips across this crossing"
        | v == "licensed-widening" = "the callee's representation contains the caller's range (widening); a stub must convert explicitly"
        | v == "licensed-with-runtime-checks" = "one side is dynamic; a generated stub must check values at the crossing"
        | otherwise = "the crossing is not licensed (" <> v <> "); a stub generator must refuse"
      licDetails =
        concat [ (label <> ": " <> v) : ax | (label, v, ax) <- posAll ]
          <> case licVerdict of
            Just v -> ["Representation dictionary: " <> overallLine v <> "."]
            Nothing -> []
      details =
        concat
          [ if arA /= arB then maybe [] (\(x, y) -> ["Arity differs: " <> x <> " vs " <> y <> "."]) (mkPair arA arB) else [],
            if bothFn && effA /= effB
              then
                [ "Effect rows differ: caller side is {" <> effA <> "}, callee side is {" <> effB <> "}.",
                  "A cross-language call would need an effect reconciliation (see organ-tool-plan Phase C)."
                ]
              else [],
            if not (isJust fnA) || not (isJust fnB)
              then ["One side is not a function type; the boundary shapes are not comparable."]
              else [],
            if ta == tb then ["Types are structurally identical."] else ["Type JSONs differ; both sides are stored verbatim in the index (ty_json) for a future checker to unify."],
            licDetails
          ]
      verdict
        -- Structural impossibilities outrank licensing: an arity or
        -- effect mismatch makes the call impossible whatever the
        -- representations allow (and zip-truncation would otherwise
        -- license a wrong-arity pair).
        | bothFn && (arA /= arB || effA /= effB) = "mismatch"
        | Just v <- licVerdict, "unlicensed" `T.isPrefixOf` v = "unlicensed-boundary"
        | ta == tb = "identical"
        | Just v <- licVerdict = v
        | otherwise = "differs-in-shape"
   in BoundaryReport
        { brA = args.ocbModuleA <> "/" <> args.ocbNameA <> " : " <> ha,
          brB = args.ocbModuleB <> "/" <> args.ocbNameB <> " : " <> hb,
          brArity = (,) <$> arA <*> arB,
          brEffectDiff = effDiff,
          brVerdict = verdict,
          brDetail = details
        }
  where
    fnOf v = case v of
      A.Object o | Just fn <- KM.lookup "fn" o -> Just fn
      A.Object o | Just fv <- KM.lookup "forall" o -> case fv of
        A.Object b | Just body <- KM.lookup "body" b -> fnOf body
        _ -> Nothing
      _ -> Nothing
    arrToList :: A.Array -> [A.Value]
    arrToList = foldr (:) []
    effectNames mfn = case mfn of
      Just (A.Object o) -> maybe "" effectRowText (KM.lookup "effect" o)
      _ -> ""
    mkPair (Just x) (Just y) = Just (T.pack (show x), T.pack (show y))
    mkPair _ _ = Nothing

-- | Input for 'organ_plan_stub'. The same four identifiers as
-- 'OrganCheckBoundary': the caller and callee symbols the generated
-- glue will connect.
data OrganPlanStub = OrganPlanStub
  { opsModuleA :: Text,
    opsNameA :: Text,
    opsLangA :: Maybe Text,
    opsModuleB :: Text,
    opsNameB :: Text,
    opsLangB :: Maybe Text
  }
  deriving (Eq, Show, Generic, JSONSchema, A.FromJSON, A.ToJSON)

instance HasFunctionObject OrganPlanStub where
  getFunctionName = "organ_plan_stub"
  getFunctionDescription =
    "Generate C-ABI glue for a cross-language call, licensed by the representation dictionary. "
      <> "Refuses crossings the axioms cannot prove safe — the refusal, with cited axioms, is the output. "
      <> "Rendered text is deterministic so a diff review of generated glue is meaningful."
  getFieldDescription "opsModuleA" = "Module of the caller symbol"
  getFieldDescription "opsNameA" = "Name of the caller symbol"
  getFieldDescription "opsLangA" = "Optional caller language when the name exists in several (e.g. haskell)"
  getFieldDescription "opsModuleB" = "Module of the callee symbol"
  getFieldDescription "opsNameB" = "Name of the callee symbol"
  getFieldDescription "opsLangB" = "Optional callee language"
  getFieldDescription _ = ""

data OrganPlanStubOutput = OrganPlanStubOutput
  { opsoVerdict :: Text,
    opsoCaller :: Text,
    opsoCallee :: Text,
    opsoStubs :: [Text]
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

-- | Unwrap the plan tool's output. (The data-instance constructor
-- stays internal; callers and tests use this accessor.)
organPlanStubOutput :: Output OrganPlanStub -> OrganPlanStubOutput
organPlanStubOutput (OrganPlanStubOut o) = o

instance Tool OrganPlanStub where
  data Output OrganPlanStub = OrganPlanStubOut OrganPlanStubOutput
    deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

  toolExec args = do
    idx <- liftIO defaultOrganIndex
    mside <- liftIO $ withIndex idx $ \conn -> do
      a <- fetchOne conn (args.opsModuleA) (args.opsNameA) (args.opsLangA)
      b <- fetchOne conn (args.opsModuleB) (args.opsNameB) (args.opsLangB)
      return ((,) <$> a <*> b)
    return $ OrganPlanStubOut $ case mside of
      Left problem ->
        OrganPlanStubOutput "unlicensed-resolve" "" "" ["// STUB REFUSED: unlicensed-resolve", "//   " <> problem]
      Right ((ta, _ha, la), (tb, _hb, lb)) ->
        case (fnOf ta, fnOf tb) of
          (Just fa, Just fb) ->
            let memberFor l t =
                  let r = tyHeadName t
                      (m, n) = T.breakOnEnd "/" r
                   in fromMaybe (D.Member D.FDynamic Nothing ("unresolved qname: " <> r)) (if T.null r || T.null m then Nothing else D.memberOf l (T.dropEnd 1 m) n)
                positions =
                  [ S.Position ("arg " <> T.pack (show i)) (memberFor la x) (memberFor lb y)
                  | (i, (x, y)) <- zip [1 :: Int ..] (zip (argsOf fa) (argsOf fb))
                  ]
                    <> case (resOf fb, resOf fa) of
                      (Just r, Just c) -> [S.Position "result" (memberFor lb r) (memberFor la c)]
                      _ -> []
                req = S.StubRequest
                  { S.srCaller = la <> ":" <> args.opsModuleA <> "/" <> args.opsNameA,
                    S.srCallee = lb <> ":" <> args.opsModuleB <> "/" <> args.opsNameB,
                    S.srPositions = positions
                  }
             in case S.planBoundary req of
                  S.StubRefused v reasons -> OrganPlanStubOutput v "" "" (S.renderCStubs (S.StubRefused v reasons))
                  plan@S.StubPlan {} ->
                    OrganPlanStubOutput
                      (S.spVerdict plan)
                      (T.intercalate "\n" (S.spCallerSide plan))
                      (T.intercalate "\n" (S.spCalleeSide plan))
                      (S.renderCStubs plan)
          _ ->
            OrganPlanStubOutput "unlicensed-shape" "" "" ["// STUB REFUSED: unlicensed-shape", "//   one side is not a function type; there is no call to glue"]
    where
      fetchOne conn m n mlang = do
        let sql = case mlang of
              Just _ -> "SELECT ty_json, type_headline, lang FROM symbols WHERE module = ? AND name = ? AND lang = ?"
              Nothing -> "SELECT ty_json, type_headline, lang FROM symbols WHERE module = ? AND name = ?"
            params = [PersistText m, PersistText n] <> maybe [] ((: []) . PersistText) mlang
        rows <- queryRows conn sql params
          (\r -> return (pvText (r !! 0), pvText (r !! 1), pvText (r !! 2)))
        return $ case rows of
          [] -> Left "Symbol not in the index; run organ_ingest first or check spelling."
          [(tj, h, l)] ->
            case A.decode (BL.fromStrict (TEnc.encodeUtf8 tj)) of
              Just v -> Right (v, h, l)
              Nothing -> Left "Stored type JSON failed to re-decode (index corruption?)."
          multiple ->
            Left
              ( "Ambiguous: symbol exists in "
                  <> T.pack (show (length multiple))
                  <> " languages ("
                  <> T.intercalate ", " [l | (_, _, l) <- multiple]
                  <> "). Re-run with the lang qualifier."
              )
      fnOf v = case v of
        A.Object o | Just fn <- KM.lookup "fn" o -> Just fn
        A.Object o | Just fv <- KM.lookup "forall" o -> case fv of
          A.Object b | Just body <- KM.lookup "body" b -> fnOf body
          _ -> Nothing
        _ -> Nothing
      argsOf fn = case fn of
        A.Object o -> case KM.lookup "args" o of
          Just (A.Array as) -> [t | A.Object a <- foldr (:) [] as, Just t <- [KM.lookup "type" a]]
          _ -> []
        _ -> []
      resOf fn = case fn of
        A.Object o -> KM.lookup "result" o
        _ -> Nothing

-- | Input for 'organ_diagnostics'.
data OrganDiagnostics = OrganDiagnostics ()
  deriving (Eq, Show, Generic, JSONSchema, A.FromJSON, A.ToJSON)

instance HasFunctionObject OrganDiagnostics where
  getFunctionName = "organ_diagnostics"
  getFunctionDescription = "Show what the shims reported during the last ingests: which files failed extraction and why. The machine-facing version of build errors."
  getFieldDescription _ = ""

data OrganDiagnosticsOutput = OrganDiagnosticsOutput
  { odLines :: [Text]
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

instance Tool OrganDiagnostics where
  data Output OrganDiagnostics = OrganDiagnosticsOut OrganDiagnosticsOutput
    deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

  toolExec _ = do
    idx <- liftIO defaultOrganIndex
    rows <- liftIO $ withIndex idx $ \conn ->
      queryRows conn
        "SELECT source_file, severity, message FROM diagnostics ORDER BY rowid DESC LIMIT 100"
        []
        (\r -> return (pvMaybeText (r !! 0), pvText (r !! 1), pvText (r !! 2)))
    return $ OrganDiagnosticsOut (OrganDiagnosticsOutput [fromMaybe "?" f <> " [" <> s <> "] " <> m | (f, s, m) <- rows])
