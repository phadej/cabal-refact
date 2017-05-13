{-# LANGUAGE OverloadedStrings #-}
module Distribution.Refact.Refactoring.IncreaseRevision (
    increaseRevisionRefactoring,
    setRevisionRefactoring,
    ) where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Refactoring
import Distribution.Refact.Types.Structure

increaseRevisionRefactoring :: Refactoring
increaseRevisionRefactoring = updateRevisionRefactoring $ maybe 1 (+1)

setRevisionRefactoring :: Int -> Refactoring
setRevisionRefactoring = updateRevisionRefactoring . const

updateRevisionRefactoring :: (Maybe Int -> Int) -> Refactoring
updateRevisionRefactoring upd fs = case fs ^? topLevelField fieldName of
    -- no revision, use zero
    Nothing -> (review _InRField <$> insertAfterVersion as) <> bs
    -- otherwise, update
    Just _ -> fs & topLevelField fieldName . _FieldNumber . _2 %~ upd . Just
  where
    fieldName :: Text
    fieldName = "x-revision"

    -- fields and rest
    (as, bs) = spanMaybe (preview _InRField) fs

    ver :: Int
    ver = upd Nothing

    ver' :: FieldValue D
    ver' = FieldNumber (D 0 $ length (fieldName ^. unpacked) + 3) ver

    insertAfterVersion [] = [(Name (D 1 0) fieldName, mempty, ver')]
    insertAfterVersion (f@(Name d n, d', fls) : rest)
        | n == "version"
            = f
            : (Name (d & dLine %~ max 1) fieldName, d', mimic d $ firstOf folded fls)
            : rest
        | otherwise = f : insertAfterVersion rest

    mimic d Nothing  = FieldNumber (d <> D 0 2) ver
    mimic _ (Just d) = FieldNumber d ver
