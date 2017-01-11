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
data Opts = Opts
    { optsDry :: !Bool
    }
  deriving (Show)

-- TODO: use own data type(s), not 'Bool'

action :: O.Parser (IO ())
action = (\o c a -> c o a) <$> opts <*> O.subparser cmds <*> args
  where
    cmds :: O.Mod O.CommandFields Action
    cmds = mconcat
        [ command "identity" "Identity refactoring, should be no-op"
            $ pure identityAction
        , command "increase-revision" "Increase the x-revision field"
            $ pure increaseRevisionAction
        , command "set-revision" "Set the x-revision field"
            $ setRevisionAction <$> rev
        , command "populate-extra-source-files"
            "Populate extra-source-field using specified globs"
            $ pure populateExtraSourceFilesAction
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
    opts = Opts
        <$> optsDryParser
      where
        optsDryParser  = fromMaybe True . lastOf folded <$> many optsDryParser'
        optsDryParser' = O.flag' True flagDry <|> O.flag' False flagInplace
        flagDry = O.short 'd' <> O.long "dry"
            <> O.help "Dry run - do not modify files"
        flagInplace = O.short 'i' <> O.long "inplace"
            <> O.help "Modify files in place"

    rev :: O.Parser Int
    rev = O.argument O.auto $ mconcat
        [ O.metavar ":revision"
        ]

-------------------------------------------------------------------------------
-- Refactorings
-------------------------------------------------------------------------------

type Action = Opts -> [FilePath] -> IO ()

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
refactMany r opts = traverse_ (refact r opts)

refact :: Refactoring -> Opts -> FilePath -> IO ()
refact r opts fp = do
    input <- (<> "\n") . T.strip <$> T.readFile fp
    fields <- fromResult (parseFields fp input)
    let fields' = posFields . r . delFields $ fields
    let output = prettyFields fields'
    displayDiff input output
    case () of
        _ | output == input -> pure ()
        _ | optsDry opts    -> putStrLn "INFO: Dry run, not modifying file"
        _ | otherwise       -> do
            putStrLn $ "Writing back to " <> fp
            T.writeFile fp output
  where
    fromResult (Success a)  = pure a
    fromResult (Failure xs) = do
        displayDoc $ _errDoc xs
        fail "PARSE ERROR"

refactManyWith :: (FilePath -> IO b) -> (b -> Refactoring) -> Action
refactManyWith f r opts = traverse_ (refactWith f r opts)

refactWith :: (FilePath -> IO b) -> (b -> Refactoring) -> Opts -> FilePath -> IO ()
refactWith f r opts fp = do
    b <- f fp
    refact (r b) opts fp

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
