{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact

import System.FilePath (takeDirectory)

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Options.Applicative as O

-------------------------------------------------------------------------------
-- Command line options
-------------------------------------------------------------------------------

-- | Common options
type Opts = () {- TODO: implement --dry #-}

action :: O.Parser (IO ())
action = flip ($) <$> opts <*> O.subparser cmds
  where
    cmds :: O.Mod O.CommandFields (Opts -> IO ())
    cmds = mconcat
        [ command "identity" "Identity refactoring, should be no-op"
            (identityAction <$> args)
        , command "increase-revision" "Increase the x-revision field"
            (increaseRevisionAction <$> args)
        , command "set-revision" "Set the x-revision field"
            (setRevisionAction <$> rev <*> args)
        , command "populate-extra-source-files"
             "Populate extra-source-field using specified globs"
            (populateExtraSourceFilesAction <$> args)
        ]

    command c d p = O.command c $ O.info (O.helper <*> p) $ mconcat
        [ O.fullDesc
        , O.progDesc d
        ]

    args :: O.Parser [FilePath]
    args = many $ O.strArgument $ mconcat
        [ O.metavar "pkg.cabal"
        ]

    opts :: O.Parser Opts
    opts = pure ()

    rev :: O.Parser Int
    rev = O.argument O.auto $ mconcat
        [ O.metavar ":revision"
        ]

-------------------------------------------------------------------------------
-- Refactorings
-------------------------------------------------------------------------------

type Action = [FilePath] -> Opts -> IO ()

identityAction :: Action
identityAction = refactMany identityRefactoring

increaseRevisionAction :: Action
increaseRevisionAction = refactMany increaseRevisionRefactoring

setRevisionAction :: Int -> Action
setRevisionAction = refactMany . setRevisionRefactoring

populateExtraSourceFilesAction :: Action
populateExtraSourceFilesAction = refactManyWith
    (getDirectoryContentsRecursive' notDistsStackWork . takeDirectory)
    populateExtraSourceFilesRefactoring

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

refactMany :: Refactoring -> Action
refactMany r fps _ = traverse_ (refact r) fps

refact :: Refactoring -> FilePath -> IO ()
refact r fp = do
    input <- (<> "\n") . T.strip <$> T.readFile fp
    fields <- fromResult (parseFields fp input)
    let fields' = posFields . r . delFields $ fields
    let output = prettyFields fields'
    displayDiff input output
  where
    fromResult (Success a)  = pure a
    fromResult (Failure xs) = do
        displayDoc $ _errDoc xs
        fail "PARSE ERROR"

refactManyWith :: (FilePath -> IO b) -> (b -> Refactoring) -> Action
refactManyWith f r fps opts = traverse_ (refactWith f r opts) fps

refactWith :: (FilePath -> IO b) -> (b -> Refactoring) -> Opts -> FilePath -> IO ()
refactWith f r _opts fp = do
    b <- f fp
    refact (r b) fp

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = join (O.execParser opts)
  where
    opts = O.info (O.helper <*> action) $ mconcat
        [ O.fullDesc
        , O.progDesc "Programmatically edit cabal files"
        , O.header "cabal-refact - edit cabal files"
        ]
