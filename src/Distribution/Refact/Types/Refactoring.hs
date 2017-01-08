module Distribution.Refact.Types.Refactoring where

import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Structure

-- | Refactoring is simply a function from a list of @'Field' 'D'@ to itself.
type Refactoring = [Field D] -> [Field D]
