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
import Distribution.Refact.Types.Version

-- | Make 'Text' from 'Field's, trying to respect 'Pos' annotations
-- as well as possible.
prettyFields :: Fields Pos -> Text
prettyFields = view strict . (<> "\n") . execW . prettyFields'

prettyFields' :: Fields Pos -> W ()
prettyFields' = traverse_ prettyField

prettyField :: Sum Comment Field Pos -> W ()
prettyField (InR (Field n p fs)) = do
    prettyName n
    prettyTextAt p ":"
    prettyFieldValue fs
prettyField (InR (Section n t fs)) = do
    prettyName n
    when (isn't _Empty t) $ do
        prettyText " "
        prettyText t
    prettyFields' fs
prettyField (InL (Comment p t)) = do
    prettyTextAt p t

prettyName :: Name Pos -> W ()
prettyName (Name p t) = prettyTextAt p t

prettyFieldValue :: FieldValue Pos -> W ()
prettyFieldValue (FieldLines fls)   = traverse_ prettyFieldLine fls
prettyFieldValue (FieldNumber p n)  = prettyTextAt p (n ^. re _Show . packed)
prettyFieldValue (FieldVersion p v) = prettyTextAt p (versionToText v)

prettyFieldLine :: FieldLine Pos -> W ()
prettyFieldLine (FieldLine p t) = prettyTextAt p t
