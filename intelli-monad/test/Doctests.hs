-- | Doctest runner for the library's copy-paste documentation surface.
--
-- Module lists are hardcoded on purpose (the documented doctest-runner
-- pattern for small, stable lists): it works identically under cabal,
-- stack, and nix. When a module is added to the library, add it here so
-- its examples stay verified.
--
-- Plain @doctest@ spawns its own GHCi, which cannot see cabal's private
-- in-place packages. Cabal can emit a package-environment file covering
-- them (@write-ghc-environment-files: always@ in cabal.project), but it
-- writes that file to the project root while GHCi only reads one from
-- the process's working directory — and cabal runs test executables from
-- the package directory. Worse, the @package-db@ paths inside that file
-- are relative to the project root too. This runner bridges all of it:
-- unless the caller already set @GHC_ENVIRONMENT@, it walks up from the
-- cwd to the nearest @.ghc.environment.*@ file, rewrites the module list
-- against the original cwd, and changes into the environment file's
-- directory so its relative @package-db@ paths resolve.
module Main (main) where

import Data.List (isPrefixOf)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>), isAbsolute, takeDirectory)
import Test.DocTest (doctest)

-- | Library modules carrying doctest examples, relative to the package
-- directory (this runner's cwd under @cabal test@).
libraryModules :: [FilePath]
libraryModules =
  [ "src/IntelliMonad/MCP/Framing.hs"
  , "src/IntelliMonad/MCP/Correlate.hs"
  , "src/IntelliMonad/MCP/Negotiate.hs"
  ]

-- | Extensions the library is compiled with; example import groups need
-- them to match.
ghcFlags :: [String]
ghcFlags = ["-XHaskell2010", "-XOverloadedStrings", "-package=intelli-monad"]

main :: IO ()
main = do
  existing <- lookupEnv "GHC_ENVIRONMENT"
  case existing of
    Just _ ->
      -- Caller manages the environment; run from here with relative paths.
      doctest (libraryModules ++ ghcFlags)
    Nothing -> do
      found <- findGhcEnvironment
      case found of
        Nothing -> doctest (libraryModules ++ ghcFlags)
        Just envFile -> do
          origDir <- getCurrentDirectory
          -- Module paths must survive the chdir below.
          let absModules =
                [ if isAbsolute m then m else origDir </> m
                | m <- libraryModules
                ]
          setCurrentDirectory (takeDirectory envFile)
          setEnv "GHC_ENVIRONMENT" envFile
          doctest (absModules ++ ghcFlags)

-- | Nearest @.ghc.environment.*@ file at or above the current directory,
-- if any. Stops at the filesystem root.
findGhcEnvironment :: IO (Maybe FilePath)
findGhcEnvironment = go =<< getCurrentDirectory
  where
    go dir = do
      entries <- listDirectory dir
      case filter (".ghc.environment." `isPrefixOf`) entries of
        (f : _) -> do
          let p = dir </> f
          ok <- doesFileExist p
          pure (if ok then Just p else Nothing)
        [] ->
          let parent = takeDirectory dir
           in if parent == dir then pure Nothing else go parent
