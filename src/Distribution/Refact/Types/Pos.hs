module Distribution.Refact.Types.Pos where

import Prelude ()
import Distribution.Refact.Internal.Prelude

-------------------------------------------------------------------------------
-- Pos
-------------------------------------------------------------------------------

data Pos = Pos !Int Int
  deriving (Eq, Ord, Show)

posLine :: Lens' Pos Int
posLine f (Pos l c) = flip Pos c <$> f l

posColumn :: Lens' Pos Int
posColumn f (Pos l c) = Pos l <$> f c

-------------------------------------------------------------------------------
-- SrcSpan
-------------------------------------------------------------------------------

data SrcSpan = SS !Pos !Pos
  deriving (Eq, Show)

instance Semigroup SrcSpan where
    SS a b <> SS c d = SS (min a c) (max b d)

ssStart :: Lens' SrcSpan Pos
ssStart f (SS s e) = flip SS e <$> f s

ssEnd :: Lens' SrcSpan Pos
ssEnd f (SS s e) = SS s <$> f e

-------------------------------------------------------------------------------
-- D
-------------------------------------------------------------------------------

-- | Position delta
data D = D !Int !Int
  deriving (Eq, Show)

dLine :: Lens' D Int
dLine f (D l c) = flip D c <$> f l

dColumn :: Lens' D Int
dColumn f (D l c) = D l <$> f c

diffPos :: Pos -> Pos -> D
diffPos (Pos a b) (Pos c d) = D (c - a) (d - b)

addPos :: Pos -> D -> Pos
addPos (Pos a b) (D c d) = Pos (a + c) (b + d)

-- | 'D' is naturally monoid, /Note:/ 'Pos' isn't.
instance Monoid D where
    mempty = D 0 0
    mappend = (<>)

instance Semigroup D where
    D a b <> D c d = D (a + c) (b + d)
