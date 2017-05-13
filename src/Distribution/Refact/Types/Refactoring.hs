module Distribution.Refact.Types.Refactoring where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Structure

-- | Refactoring is simply a function from a list of @'Field' 'D'@ to itself.
type Refactoring = Fields D -> Fields D

topLevelField :: Applicative f => Text -> LensLike' f (Fields D) (FieldValue D)
topLevelField n = traverse . _InRField . filtered (\t -> t ^. _1 . nameText == n) . _3
