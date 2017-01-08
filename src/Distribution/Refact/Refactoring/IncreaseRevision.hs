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
    Nothing -> (review _Field <$> insertAfterVersion as) <> bs
    -- otherwise, update
    Just _ -> fs & topLevelField fieldName %~ updateRevision
  where
    fieldName :: Text
    fieldName = "x-revision"

    -- fields and rest
    (as, bs) = spanMaybe (preview _Field) fs

    ver :: Text
    ver = upd Nothing ^. re _Show . packed

    ver' :: [FieldLine D]
    ver' = [FieldLine (D 0 $ length (fieldName ^. unpacked) + 3) ver]

    insertAfterVersion [] = [(Name (D 1 0) fieldName, mempty, ver')]
    insertAfterVersion (f@(Name d n, d', fls) : rest)
        | n == "version"
            = f
            : (Name (d & dLine %~ max 1) fieldName, d', mimic fls d')
            : rest
        | otherwise = f : insertAfterVersion rest

    mimic []                  d = [FieldLine (d <> D 0 2) ver]
    mimic (FieldLine d _ : _) _ = [FieldLine d ver]

    updateRevision []                  = ver'
    updateRevision (FieldLine d v : _) =
        let v' = upd (v ^? unpacked . _Show) ^. re _Show . packed
        in [FieldLine d v']

topLevelField :: Applicative f => Text -> LensLike' f [Field D] [FieldLine D]
topLevelField n = traverse . _Field . filtered (\t -> t ^. _1 . nameText == n) . _3

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe _ xs@[]       =  ([], xs)
spanMaybe p xs@(x:xs') = case p x of
    Nothing -> ([], xs)
    Just y  -> let (ys, zs) = spanMaybe p xs' in (y:ys,zs)
