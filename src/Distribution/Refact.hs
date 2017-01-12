{-# LANGUAGE OverloadedStrings #-}
module Distribution.Refact (
    -- * Types
    module Distribution.Refact.Types.Pos,
    module Distribution.Refact.Types.Structure,
    module Distribution.Refact.Types.Version,
    -- * Pretty
    prettyFields,
    -- * Parsing
    parseFields,
    -- * Annotations
    delFields,
    posFields,
    -- * Edit
    displayDiff,
    diffShowS,
    -- * Refactorings
    Refactoring,
    identityRefactoring,
    increaseRevisionRefactoring,
    setRevisionRefactoring,
    populateExtraSourceFilesRefactoring,
    setVersionRefactoring,
    -- * Internal helpers
    displayDoc,
    Result (..),
    _errDoc,
    getDirectoryContentsRecursive',
    notDistsStackWork,
    ) where

import Prelude ()

import Distribution.Refact.Annotations
import Distribution.Refact.Parser
import Distribution.Refact.Pretty
import Distribution.Refact.Refactoring.Identity
import Distribution.Refact.Refactoring.IncreaseRevision
import Distribution.Refact.Refactoring.PopulateExtraSourceFiles
import Distribution.Refact.Refactoring.SetVersion
import Distribution.Refact.Tools.Edit
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Refactoring
import Distribution.Refact.Types.Structure
import Distribution.Refact.Types.Version

import Text.Trifecta (Result (..), _errDoc)
