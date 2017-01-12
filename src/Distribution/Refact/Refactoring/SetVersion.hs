{-# LANGUAGE OverloadedStrings #-}
module Distribution.Refact.Refactoring.SetVersion (
    setVersionRefactoring,
    ) where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Refactoring
import Distribution.Refact.Types.Structure
import Distribution.Refact.Types.Version

setVersionRefactoring :: Version -> Refactoring
setVersionRefactoring = updateVersionRefactoring . const

updateVersionRefactoring :: (Maybe Version -> Version) -> Refactoring
updateVersionRefactoring upd fs = case fs ^? topLevelField fieldName of
    -- no revision, use zero
    Nothing -> (review _Field  <$> insertAfterName as) <> bs
    -- otherwise, update
    Just _ -> fs & topLevelField fieldName %~ upd . Just
  where
    fieldName :: Text
    fieldName = "version"

    -- fields and rest
    (as, bs) = spanMaybe (preview _Field) fs

    ver :: Version
    ver = upd Nothing

    ver' :: FieldValue D
    ver' = FieldVersion (D 0 $ length (fieldName ^. unpacked) + 3) ver

    insertAfterName [] = [(Name (D 1 0) fieldName, mempty, ver')]
    insertAfterName (f@(Name d n, d', fls) : rest)
        | n == "name"
            = f
            : (Name (d & dLine %~ max 1) fieldName, d', mimic d $ firstOf folded fls)
            : rest
        | otherwise = f : insertAfterName rest

    mimic d Nothing  = FieldVersion (d <> D 0 2) ver
    mimic _ (Just d) = FieldVersion d ver

topLevelField :: Applicative f => Text -> LensLike' f [Field D] Version
topLevelField n = traverse . _Field . filtered (\t -> t ^. _1 . nameText == n) . _3 . _FieldVersion . _2

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe _ xs@[]       =  ([], xs)
spanMaybe p xs@(x:xs') = case p x of
    Nothing -> ([], xs)
    Just y  -> let (ys, zs) = spanMaybe p xs' in (y:ys,zs)
