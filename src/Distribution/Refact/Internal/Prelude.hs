module Distribution.Refact.Internal.Prelude (
    module Prelude,
    module Control.Lens,
    -- * Control.Applicative
    Alternative (..),
    optional,
    -- * Control.Monad
    join, when,
    -- * Data.Functor
    void,
    -- * Data.Int
    Int64,
    -- * Data.Foldable
    traverse_, for_,
    -- * Data.List
    sortBy, sortOn,
    -- * Data.Maybe
    fromMaybe, listToMaybe,
    -- * Data.Monoid
    Endo (..),
    -- * bifunctors
    Bifunctor (..),
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
    ) where

import Prelude

-- We endorse lens
import Control.Lens

import Control.Applicative        (Alternative (..), optional)
import Control.Monad              (join, when)
import Control.Monad.IO.Class
import Data.Align                 (Align (..))
import Data.Bifunctor             (Bifunctor (..))
import Data.Foldable              (for_, traverse_)
import Data.Functor               (void)
import Data.Int                   (Int64)
import Data.List                  (sortBy, sortOn)
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
