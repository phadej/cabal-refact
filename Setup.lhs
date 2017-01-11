#!/usr/bin/runhaskell
\begin{code}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List ( nub )
#if MIN_VERSION_Cabal(1,24,0)
import Distribution.Package ( PackageId, UnitId (..),  ComponentId (..) )
#else
import Distribution.Package ( PackageName(PackageName), PackageId, InstalledPackageId(InstalledPackageId), packageVersion, packageName )
import Data.Version ( showVersion )
#endif
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag)
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps), compiler, buildDir)
import Distribution.Simple.Compiler (showCompilerId)
import Distribution.Verbosity ( Verbosity )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  let bdir = buildDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
        [ "module Build_" ++ testName suite ++ " where"
        , ""
        , "autogen_dir :: String"
        , "autogen_dir = " ++ show dir
        , ""
        , "build_dir :: String"
        , "build_dir = " ++ show bdir
        , ""
        , "deps :: [String]"
        , "deps = " ++ (show $ formatDeps (testDeps libcfg suitecfg))
        , ""
        , "compiler :: String"
        , "compiler = " ++ (show $ showCompilerId $ compiler lbi)
        ]


#if MIN_VERSION_Cabal(1,24,0)
testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(UnitId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

formatDeps :: [(UnitId, a)] -> [String]
formatDeps = map (formatone . fst)
  where
    formatone (SimpleUnitId (ComponentId i)) = i
#else
testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

formatDeps :: [(InstalledPackageId, a)] -> [String]
formatDeps = map (formatone . fst)
  where
    formatone (InstalledPackageId i) = i
#endif

\end{code}
