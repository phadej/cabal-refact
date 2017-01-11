{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main (doctests)
-- Copyright   :  (C) 2012-14 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides doctests for a project based on the actual versions
-- of the packages it was built with. It requires a corresponding Setup.lhs
-- to be added to the project
-----------------------------------------------------------------------------
module Main where

import Build_doctests (autogen_dir, build_dir, deps, compiler)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Test.DocTest

##if defined(mingw32_HOST_OS)
##if defined(i386_HOST_ARCH)
##define USE_CP
import Control.Applicative
import Control.Exception
import Foreign.C.Types
foreign import stdcall "windows.h SetConsoleCP" c_SetConsoleCP :: CUInt -> IO Bool
foreign import stdcall "windows.h GetConsoleCP" c_GetConsoleCP :: IO CUInt
##elif defined(x86_64_HOST_ARCH)
##define USE_CP
import Control.Applicative
import Control.Exception
import Foreign.C.Types
foreign import ccall "windows.h SetConsoleCP" c_SetConsoleCP :: CUInt -> IO Bool
foreign import ccall "windows.h GetConsoleCP" c_GetConsoleCP :: IO CUInt
##endif
##endif

-- | Run in a modified codepage where we can print UTF-8 values on Windows.
withUnicode :: IO a -> IO a
##ifdef USE_CP
withUnicode m = do
  cp <- c_GetConsoleCP
  (c_SetConsoleCP 65001 >> m) `finally` c_SetConsoleCP cp
##else
withUnicode m = m
##endif

main :: IO ()
main = withUnicode $ getSources >>= \sources -> do
    -- Let us find potential databases, cabal new-build
    home <- getHomeDirectory
    cwd  <- getCurrentDirectory
    let storedb   = home </> ".cabal" </> "store" </> compiler </> "package.db"
    let localdb   = cwd </> "dist-newstyle" </> "packagedb" </> compiler
    let inplacedb = build_dir </> "package.conf.inplace"
    -- Note: we could add sandbox directories as well,
    -- but let us assume everyone is switching to new-build
    dbs <- filterM doesDirectoryExist [storedb,localdb,inplacedb]
    -- Run doctests
    doctest (args sources dbs)
  where
    args sources dbs
      = "-isrc"
      : "-no-user-package-db"
      : map ("-package-db="++) dbs
      ++  ("-i" ++ autogen_dir)
      : "-optP-include"
      : ("-optP" ++ autogen_dir ++ "/cabal_macros.h")
      : "-hide-all-packages"
#ifdef TRUSTWORTHY
      : "-DTRUSTWORTHY=1"
#endif
      : map ("-package-id="++) deps
      ++ sources

getSources :: IO [FilePath]
getSources = filter (isSuffixOf ".hs") <$> go "src"
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
