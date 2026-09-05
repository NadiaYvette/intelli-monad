{-# LANGUAGE CPP #-}
-- | Doctest runner for the library's copy-paste documentation surface.
--
#include "ghcversion.h"
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
-- are relative to the project root too, and the root accumulates one
-- env file per GHC version. This runner bridges all of it: unless the
-- caller already set @GHC_ENVIRONMENT@, it constructs the exact file for
-- the GHC this test binary was linked with (@compilerVersion@ — the
-- same GHC cabal selected for the build), rewrites the module list
-- against the original cwd, and changes into the environment file's
-- directory so its relative @package-db@ paths resolve.
module Main (main) where

import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>), isAbsolute, takeDirectory)
import System.Info (arch, os)
import Test.DocTest (doctest)

-- | Library modules carrying doctest examples, relative to the package
-- directory (this runner's cwd under @cabal test@).
libraryModules :: [FilePath]
libraryModules =
  [ "src/IntelliMonad/MCP/Framing.hs"
  , "src/IntelliMonad/MCP/Correlate.hs"
  , "src/IntelliMonad/MCP/Negotiate.hs"
  , "src/IntelliMonad/MCP/Wire.hs"
  , "src/IntelliMonad/Tools/OrganBank/Dictionary.hs"
  , "src/IntelliMonad/Tools/OrganBank/Stubs.hs"
  ]

-- | Extensions the library is compiled with; example import groups need
-- them to match.
ghcFlags :: [String]
ghcFlags = ["-XHaskell2010", "-XOverloadedStrings", "-package=intelli-monad"]

-- | The exact env file for this GHC, e.g.
-- @.ghc.environment.x86_64-linux-9.8.4@ for a test binary linked by
-- GHC 9.8.4. Under @cabal test@ the linking GHC is the one cabal wrote
-- the env file for, so this is deterministic; if the file is missing
-- (hand-built binary, or the project flag was disabled) we fail loudly
-- instead of guessing. The full X.Y.Z version comes from GHC's own
-- @ghcversion.h@ — 'System.Info.compilerVersion' only reports the
-- bootstrap version (9.8), which cabal's filename does not use.
envFileName :: String
envFileName =
  ".ghc.environment." ++ arch ++ "-" ++ os ++ "-" ++ ghcFullVersion
  where
    ghcFullVersion :: String
    ghcFullVersion =
      show (ghcVersion `div` 100)
        ++ "."
        ++ show (ghcVersion `mod` 100)
        ++ "."
        ++ show ghcPatchLevel
    ghcVersion, ghcPatchLevel :: Int
    ghcVersion = __GLASGOW_HASKELL__
    ghcPatchLevel = __GLASGOW_HASKELL_PATCHLEVEL1__

main :: IO ()
main = do
  existing <- lookupEnv "GHC_ENVIRONMENT"
  case existing of
    Just _ ->
      -- Caller manages the environment; run from here with relative paths.
      doctest (libraryModules ++ ghcFlags)
    Nothing -> do
      origDir <- getCurrentDirectory
      let absModules =
            [ if isAbsolute m then m else origDir </> m
            | m <- libraryModules
            ]
          root = projectRoot origDir
          envFile = root </> envFileName
      ok <- doesFileExist envFile
      if not ok
        then do
          putStrLn $
            "doctests: " ++ envFileName ++ " not found in project root "
              ++ root ++ " — was the library built with write-ghc-environment-files?"
          exitFailure
        else do
          -- Module paths must survive the chdir below.
          setCurrentDirectory root
          setEnv "GHC_ENVIRONMENT" envFile
          doctest (absModules ++ ghcFlags)

-- | Project root: the directory containing the package directory (this
-- runner's cwd under @cabal test@ is @\<root\>/intelli-monad@). No
-- walk-up guessing: the env-file name above is exact, so a miss is a
-- build-state problem to report, not to paper over.
projectRoot :: FilePath -> FilePath
projectRoot = takeDirectory
