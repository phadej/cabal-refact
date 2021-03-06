{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Distribution.Refact.Types.Structure where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Types.Version

import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Field
-------------------------------------------------------------------------------

-- | Field list.
type Fields ann = [Sum Comment Field ann]

-- | A Cabal-like file consists of a series of fields (@foo: bar@)
-- and sections (@library ...@).
data Field ann
    = Field   !(Name ann) ann (FieldValue ann)
    | Section !(Name ann) !Text (Fields ann)
  deriving (Functor, Foldable, Traversable)

instance Foldable1 Field

_Field :: Prism' (Field a) (Name a, a, FieldValue a)
_Field = prism f g
  where
    f (n, ann, fls) = Field n ann fls
    g (Field n ann fls) = Right (n, ann, fls)
    g x                 = Left x

_InRField :: Prism' (Sum f Field a) (Name a, a, FieldValue a)
_InRField = _InR . _Field

-------------------------------------------------------------------------------
-- Field Values
-------------------------------------------------------------------------------

-- | The value of a field from a Cabal file.
--
-- We parse some fields into different formats, but by default they are 'FieldLines'.
data FieldValue ann
    = FieldLines [FieldLine ann]  -- ^ /Default:/ collection of non-empty lines
    | FieldNumber !ann !Int       -- e.g. @x-revision@
    | FieldVersion !ann !Version  -- e.g. @version@
  deriving (Eq, Show, Functor, Foldable, Traversable)

_FieldLines :: Prism' (FieldValue a) [FieldLine a]
_FieldLines = prism FieldLines $ \v -> case v of
    FieldLines x -> Right x
    _            -> Left v

_FieldNumber :: Prism' (FieldValue a) (a, Int)
_FieldNumber = prism (uncurry FieldNumber) $ \v -> case v of
    FieldNumber ann n -> Right (ann, n)
    _                 -> Left v

_FieldVersion :: Prism' (FieldValue a) (a, Version)
_FieldVersion = prism (uncurry FieldVersion) $ \v -> case v of
    FieldVersion ann n -> Right (ann, n)

    _                 -> Left v

-------------------------------------------------------------------------------
-- FieldLine
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

-------------------------------------------------------------------------------
-- Comment
-------------------------------------------------------------------------------

-- | Comment
--
-- /Invariant:/ 'Text' has no newlines.
data Comment ann = Comment !ann !Text
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Foldable1 Comment

-------------------------------------------------------------------------------
-- Functor classes
-------------------------------------------------------------------------------

deriveEq1 ''FieldLine
deriveEq1 ''Comment
deriveEq1 ''Name
deriveEq1 ''Field
deriveEq1 ''FieldValue

deriveShow1 ''FieldLine
deriveShow1 ''Comment
deriveShow1 ''Name
deriveShow1 ''Field
deriveShow1 ''FieldValue

deriving instance Eq ann => Eq (Field ann)
deriving instance Show ann => Show (Field ann)
