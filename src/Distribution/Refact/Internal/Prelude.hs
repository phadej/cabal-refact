module Distribution.Refact.Internal.Prelude (
    module Prelude,
    module Control.Lens,
    -- * Control.Applicative
    Alternative (..),
    optional,
    -- * Control.Monad
    join, when,
    -- * Data.Deriving
    deriveEq1, deriveShow1,
    -- * Data.Functor
    void,
    -- * Data.Functor.Classes
    Eq1 (..), Show1 (..), showsBinaryWith,
    -- * Data.Functor.Sum
    Sum (..),
    -- * Data.Int
    Int64,
    -- * Data.Foldable
    traverse_, for_,
    -- * Data.List
    sortBy, sortOn,
    -- * Data.List.NonEmpty
    NonEmpty (..), some1,
    -- * Data.Maybe
    fromMaybe, listToMaybe,
    -- * Data.Monoid
    Endo (..),
    -- * bifunctors
    Bifunctor (..),
    -- * containers
    toMapOf,
    -- * semigroups
    Semigroup (..),
    -- * semigroupoids
    Foldable1 (..),
    Traversable1 (..),
    -- * transformers
    MonadIO (..),
    -- * text
    Text,
    packed, unpacked,
    -- * these
    These (..),
    Align (..),
    -- * vector
    Vector,
    vector,
    -- * extras
    asText,
    _InR,
    spanMaybe,
    ) where

import Prelude

-- We endorse lens
import Control.Lens

import Control.Applicative        (Alternative (..), optional)
import Control.Monad              (join, when)
import Control.Monad.IO.Class
import Data.Align                 (Align (..))
import Data.Bifunctor             (Bifunctor (..))
import Data.Deriving              (deriveEq1, deriveShow1)
import Data.Foldable              (for_, traverse_)
import Data.Functor               (void)
import Data.Functor.Classes       (Eq1 (..), Show1 (..), showsBinaryWith)
import Data.Functor.Sum           (Sum (..))
import Data.Int                   (Int64)
import Data.List                  (sortBy, sortOn)
import Data.List.NonEmpty         (NonEmpty (..), some1)
import Data.Map.Lens              (toMapOf)
import Data.Maybe                 (fromMaybe, listToMaybe)
import Data.Monoid                (Endo (..))
import Data.Semigroup             (Semigroup (..))
import Data.Semigroup.Foldable    (Foldable1 (..))
import Data.Semigroup.Traversable (Traversable1 (..))
import Data.Text                  (Text)
import Data.Text.Lens             (packed, unpacked)
import Data.These                 (These (..))
import Data.Vector                (Vector)
import Data.Vector.Lens           (vector)

asText :: Show a => Getter a Text
asText = to show . packed

_InR :: Prism' (Sum f g a) (g a)
_InR = prism InR g
  where
    g (InR x) = Right x
    g x       = Left x

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe _ xs@[]       =  ([], xs)
spanMaybe p xs@(x:xs') = case p x of
    Nothing -> ([], xs)
    Just y  -> let (ys, zs) = spanMaybe p xs' in (y:ys,zs)
