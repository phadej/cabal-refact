{-# LANGUAGE OverloadedStrings #-}
module Distribution.Refact.Refactoring.PopulateExtraSourceFiles (
    populateExtraSourceFilesRefactoring,
    getDirectoryContentsRecursive',
    notDistsStackWork,
    ) where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Refactoring
import Distribution.Refact.Types.Structure

import Data.Char        (isSpace)
import Data.List        (nubBy, sort)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath  (takeFileName, (</>))
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified System.FilePath.Glob        as Glob
import qualified Text.Regex.Applicative.Text as RE

populateExtraSourceFilesRefactoring :: [FilePath] -> Refactoring
populateExtraSourceFilesRefactoring files =
    topLevelField "extra-source-files" %~ nubBy nubber . refactor
  where
    nubber (FieldLine _ n) (FieldLine _ m) = n == m

    refactor []                    = []
    refactor (f@(FieldLine _ n) : fls) = case RE.match pragmaRe n of
        Nothing  -> f : refactor fls
        Just pat ->
            let pattern = Glob.simplify $ Glob.compile $ pat ^. unpacked
                files'  = filter (Glob.match pattern) files
            in f :  map mk (sort files') ++ refactor fls
      where
        mk fp = FieldLine (D 1 0) (fp ^. packed)

    -- We could use trifecta here as well. Shouldn't we?
    -- We don't need any/good error reporting though.
    pragmaRe :: RE.RE' String -- glob pattern
    pragmaRe = "--" *> spacesRe *> "cabal-refact-populate"
        *> optional spacesRe *> RE.sym ':'
        *> optional spacesRe
        *> filenameRe
        <* optional (spacesRe *> many RE.anySym)

    filenameRe = some $ RE.psym $ not . isSpace
    spacesRe = some $ RE.psym isSpace

-- | Returns 'False' for:
--
-- * '.git'
--
-- * 'dist'
--
-- * 'dist-newstyle'
--
-- * '.stack-work'
notDistsStackWork :: FilePath -> Bool
notDistsStackWork fp = takeFileName fp `notElem`
    [ ".git"
    , "dist"
    , "dist-newstyle"
    , ".stack-work"
    ]

-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
--
-- /Note:/ From @Cabal@'s "Distribution.Simple.Utils"
getDirectoryContentsRecursive'
    :: (FilePath -> Bool) -- ^ Check, whether to recurse
    -> FilePath           -- ^ top dir
    -> IO [FilePath]
getDirectoryContentsRecursive' ignore' topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files
                                                     ,reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore x          = not (ignore' x)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

topLevelField :: Applicative f => Text -> LensLike' f [Field D] [FieldLine D]
topLevelField n = traverse . _Field . filtered (\t -> t ^. _1 . nameText == n) . _3 . _FieldLines


