{-# LANGUAGE OverloadedStrings #-}
-- | Format 'Field' back to 'Text'.
module Distribution.Refact.Pretty (
    prettyFields,
    ) where

import Prelude ()
import Distribution.Refact.Internal.Prelude

import Distribution.Refact.Tools.Pretty
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Structure

-- | Make 'Text' from 'Field's, trying to respect 'Pos' annotations
-- as well as possible.
prettyFields :: [Field Pos] -> Text
prettyFields = view strict . (<> "\n") . execW . prettyFields'

prettyFields' :: [Field Pos] -> W ()
prettyFields' = traverse_ prettyField

prettyField :: Field Pos -> W ()
prettyField (Field n p fs) = do
    prettyName n
    prettyTextAt p ":"
    prettyFieldValue fs
prettyField (Section n t fs) = do
    prettyName n
    when (isn't _Empty t) $ do
        prettyText " "
        prettyText t
    prettyFields' fs
prettyField (Comment p t) = do
    prettyTextAt p t

prettyName :: Name Pos -> W ()
prettyName (Name p t) = prettyTextAt p t

prettyFieldValue :: FieldValue Pos -> W ()
prettyFieldValue (FieldLines fls) = traverse_ prettyFieldLine fls

prettyFieldLine :: FieldLine Pos -> W ()
prettyFieldLine (FieldLine p t) = prettyTextAt p t
