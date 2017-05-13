module Distribution.Refact.Annotations (
    delFields,
    posFields,
    ) where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Structure

-- TODO: use firstOf1
import Data.Semigroup (First (..), Last (..))

-------------------------------------------------------------------------------
-- SrcSpan -> D
-------------------------------------------------------------------------------

-- | Change fields annotations from absolute 'SrcSpan' to relative 'D'.
delFields :: Fields SrcSpan -> Fields D
delFields = delFields' (Pos 0 0)

delSum :: Pos -> Sum Comment Field SrcSpan -> Sum Comment Field D
delSum p (InR f) = InR (delField p f)
delSum p (InL c) = InL (delComment p c)

delField :: Pos -> Field SrcSpan -> Field D
delField p f = case f of
    Field n ss fls -> Field
        (diffPos p p' <$ n)
        (diffPos p' $ ss ^. ssStart)
        (delFieldValue p' fls)
      where
        p' = n ^. nameAnn . ssStart
    Section n t fs -> Section
        (g <$> n)
        t
        (delFields' (n ^. nameAnn . ssStart) fs)
  where
    g (SS p' _) = diffPos p p'

delComment :: Pos -> Comment SrcSpan -> Comment D
delComment p (Comment ss t) = Comment (g ss) t
  where
    g (SS p' _) = diffPos p p'

delFieldValue :: Pos -> FieldValue SrcSpan -> FieldValue D
delFieldValue p (FieldLines fls)    = FieldLines (delFieldLines p fls)
delFieldValue p (FieldNumber ss n)  = FieldNumber (diffPos p $ ss ^. ssStart) n
delFieldValue p (FieldVersion ss v) = FieldVersion (diffPos p $ ss ^. ssStart) v

delFieldLines :: Pos -> [FieldLine SrcSpan] -> [FieldLine D]
delFieldLines _ [] = []
delFieldLines p (FieldLine ss t : fls) =
    let p' = ss ^. ssStart
    in FieldLine (diffPos p p') t : delFieldLines p' fls

delFields' :: Pos -> Fields SrcSpan -> Fields D
delFields' _p []       = []
delFields'  p (f : fs) =
  let SS b e = fold1 f
  in delSum p f : delFields' (Pos (e ^. posLine) (b ^. posColumn)) fs

-------------------------------------------------------------------------------
-- D -> Pos
-------------------------------------------------------------------------------

-- | Change fields annotations from relative 'D' to absolute 'Pos'.
posFields :: Fields D -> Fields Pos
posFields = posFields' (Pos 0 0)

posFields' :: Pos -> Fields D -> Fields Pos
posFields' _p []       = []
posFields'  p (f : fs) = f' : posFields' p'' fs
  where
    -- where to start current field
    f' = posSum p' f
    d = getFirst . foldMap1 First $ f
    p' = addPos p d

    -- offset of the next fields
    Pos l _  = getLast . foldMap1 Last $ f'
    p'' = Pos l $ p' ^. posColumn

posSum :: Pos -> Sum Comment Field D -> Sum Comment Field Pos
posSum p (InR f) = InR (posField p f)
posSum p (InL c) = InL (posComment p c)

posField :: Pos -> Field D -> Field Pos
posField p (Field n d fls) = Field
    (n & nameAnn .~ p)
    (addPos p d)
    (posFieldValue p fls)
posField p (Section n t fs) = Section
    (n & nameAnn .~ p)
    t
    (posFields' p fs)

posComment :: Pos -> Comment D -> Comment Pos
posComment p (Comment _ t) = Comment p t

posFieldValue :: Pos -> FieldValue D -> FieldValue Pos
posFieldValue p (FieldLines fls)   = FieldLines (posFieldLines p fls)
posFieldValue p (FieldNumber d n)  = FieldNumber (addPos p d) n
posFieldValue p (FieldVersion d v) = FieldVersion (addPos p d) v

posFieldLines :: Pos -> [FieldLine D] -> [FieldLine Pos]
posFieldLines _ [] = []
posFieldLines p (FieldLine d t : rest) =
    let p' = addPos p d
    in FieldLine p' t : posFieldLines p' rest
