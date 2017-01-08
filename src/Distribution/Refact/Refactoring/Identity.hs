module Distribution.Refact.Refactoring.Identity where

import Distribution.Refact.Types.Refactoring

identityRefactoring :: Refactoring
identityRefactoring = id
