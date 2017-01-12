{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact

import Data.List        (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath  ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    fixtures <- sequence
        [ identityFixtures
        , refactoringFixtures "increase-revision" increaseRevisionRefactoring
        , refactoringFixtures "populate-extra-source-files" $
            populateExtraSourceFilesRefactoring
                [ "fixtures/foo.cabal"
                , "fixtures/bar.cabal"
                , "Setup.hs"
                ]
        , refactoringFixtures "set-version" $
            setVersionRefactoring $ Version $ 1 :| [2,3,4]
        ]
    defaultMain $ testGroup "fixtures" fixtures

-------------------------------------------------------------------------------
-- Fixtures: identity
-------------------------------------------------------------------------------

identityFixtures :: IO TestTree
identityFixtures = do
    files <- filter (".cabal" `isSuffixOf`) <$> listDirectory dir
    pure $ testGroup "identity" $ map mk files
  where
    dir = "fixtures" </> "identity"

    mk fn = testCase fn $ do
        input <- (<> "\n") . T.strip <$> T.readFile (dir </> fn)
        fields <- fromResult (parseFields fn input)
        let fields' = posFields . delFields $ fields
        let output = prettyFields fields'
        when (input /= output) $ do
            assertFailure $ diffShowS input output ""

refactoringFixtures :: String -> Refactoring -> IO TestTree
refactoringFixtures name refactoring = do
    files <- filter (".cabal" `isSuffixOf`) <$> listDirectory dir
    pure $ testGroup name $ map mk files
  where
    dir = "fixtures" </> name

    mk fn = testCase fn $ do
        input <- (<> "\n") . T.strip <$> T.readFile (dir </> fn)
        expected <- (<> "\n") . T.strip <$> T.readFile (dir </> (fn ++ ".output"))
        fields <- fromResult (parseFields fn input)
        let fields' = posFields . refactoring . delFields $ fields
        let output = prettyFields fields'
        when (expected /= output) $ do
            assertFailure $ diffShowS expected output ""

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

fromResult :: Result a -> IO a
fromResult (Success a)  = pure a
fromResult (Failure xs) = do
    assertFailure $ show $ _errDoc xs
    fail "" -- to make it return 'a'
