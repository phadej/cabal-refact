{-# LANGUAGE OverloadedStrings #-}
module Distribution.Refact.Types.Version where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Tools.Trifecta

import Data.List (foldl')

newtype Version = Version (NonEmpty Word)
  deriving (Eq, Show)

versionToText :: Version -> Text
versionToText (Version (x :| xs)) = foldl' f (x ^. asText) xs
  where
    f t y = t <> "." <> y ^. asText

versionParser :: CharParsing m => m Version
versionParser = f <$> integral <*> many (char '.' *> integral) <?> "version"
  where
    f x xs = Version (x :| xs)

