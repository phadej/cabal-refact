{-# LANGUAGE DeriveTraversable #-}
module Distribution.Refact.Types.Structure where

import Prelude ()
import Distribution.Refact.Internal.Prelude

import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Field
-------------------------------------------------------------------------------

-- | A Cabal-like file consists of a series of fields (@foo: bar@)
-- and sections (@library ...@).
data Field ann
    = Field   !(Name ann) ann [FieldLine ann]
    | Section !(Name ann) !Text [Field ann]
    | Comment !ann !Text
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Foldable1 Field

_Field :: Prism' (Field a) (Name a, a, [FieldLine a])
_Field = prism f g
  where
    f (n, ann, fls) = Field n ann fls
    g (Field n ann fls) = Right (n, ann, fls)
    g x                 = Left x

-------------------------------------------------------------------------------
-- Field Values
-------------------------------------------------------------------------------

-- | A line of text representing the value of a field from a Cabal file.
-- A field may contain multiple lines.
--
-- /Invariant:/ 'Text' has no newlines.
data FieldLine ann  = FieldLine  !ann !Text
  deriving (Eq, Show, Functor, Foldable, Traversable)

fieldLineNull :: FieldLine ann -> Bool
fieldLineNull (FieldLine _ t) = T.null t

-------------------------------------------------------------------------------
-- Name
-------------------------------------------------------------------------------

-- | A field name.
--
-- TODO: (CI Text)
data Name ann  = Name !ann !Text
  deriving (Eq, Show, Functor, Foldable, Traversable)

nameAnn :: Lens (Name a) (Name b) a b
nameAnn f (Name a n) = f a <&> \b -> Name b n

nameText :: Lens (Name a) (Name a) Text Text
nameText f (Name a n) = f n <&> Name a