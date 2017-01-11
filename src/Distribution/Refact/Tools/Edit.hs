{-# LANGUAGE OverloadedStrings #-}
module Distribution.Refact.Tools.Edit where

import Prelude ()
import Distribution.Refact.Internal.Prelude

import System.IO (stdout)
import Data.Monoid (Endo (..))
import Control.Monad.Writer.Strict (execWriter, tell)

import qualified Data.Text                    as T
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI

import qualified Distribution.Refact.Tools.Edit.Algorithm as Edit

-- | move somewhere else
displayDoc :: ANSI.Doc -> IO ()
displayDoc
    = ANSI.displayIO stdout
    . ANSI.renderPretty 1.8 80
    . (ANSI.<> ANSI.linebreak)

-- | Make diff and display it.
displayDiff
    :: Text  -- ^ old
    -> Text  -- ^ new
    -> IO ()
displayDiff a b | a == b
    = displayDoc $ ANSI.cyan $ ANSI.text "no changes"
displayDiff b a =
    for_ (Edit.hunks 3 $ Edit.improve $ Edit.diff (T.lines a) (T.lines b)) $ \h -> do
        displayDoc $ ANSI.blue $ ANSI.text $ replicate 72 '='
        for_ h $ \e -> displayDoc $ case e of
            Edit.Take t -> ANSI.text $ T.unpack $ "  " <> t
            Edit.Drop t -> ANSI.red $ ANSI.text $ T.unpack $ "- " <> t
            Edit.Add t  -> ANSI.green $ ANSI.text $ T.unpack $ "+ " <> t

diffShowS
    :: Text  -- ^ old
    -> Text  -- ^ new
    -> ShowS
diffShowS a b | a == b = showString "no changes"
diffShowS b a = appEndo $ execWriter $
    for_ (Edit.hunks 3 $ Edit.improve $ Edit.diff (T.lines a) (T.lines b)) $ \h -> do
        tell $ Endo $ showString $ replicate 72 '='
        tell $ Endo $ showChar '\n'
        for_ h $ \e -> do
            tell . Endo . showString $ case e of
                Edit.Take t -> T.unpack $ "  " <> t
                Edit.Drop t -> T.unpack $ "- " <> t
                Edit.Add t  -> T.unpack $ "+ " <> t
            tell $ Endo $ showChar '\n'
