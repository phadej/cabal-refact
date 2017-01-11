module Distribution.Refact.Tools.Trifecta (
    -- * Modules
    module Text.Trifecta,
    module Text.Trifecta.Indentation,
    -- * Types
    runIParser,
    -- * Additional parsers
    spaces',
    nl,
    integral,
    ) where

import Prelude ()
import Distribution.Refact.Internal.Prelude

import Data.Char                 (ord)
import Data.List                 (foldl')
import Text.Trifecta             hiding (Parser)
import Text.Trifecta.Delta
import Text.Trifecta.Indentation hiding (IndentationParserT)

import qualified Data.Text.Encoding        as TE
import qualified Text.Trifecta             as Tri
import qualified Text.Trifecta.Indentation as Tri

-------------------------------------------------------------------------------
-- parser types
-------------------------------------------------------------------------------

runIParser
    :: Tri.IndentationParserT Char Tri.Parser a -- ^ parser
    -> FilePath                                 -- ^ filename
    -> Text                                     -- ^ contents
    -> Result a
runIParser p fn contents =
    parseByteString p' (Directed fnBS 0 0 0 0) contentsBS
  where
    contentsBS = TE.encodeUtf8 contents
    fnBS = TE.encodeUtf8 (fn ^. packed)
    s = mkIndentationState 0 infIndentation True Gt
    p' = evalIndentationParserT p s

-------------------------------------------------------------------------------
-- Additional parsers
-------------------------------------------------------------------------------

spaces' :: CharParsing m => m ()
spaces' = skipMany (oneOf " \t")

nl :: (CharParsing m, IndentationParsing m) => m ()
nl = newline *> localIndentation Any spaces

integral :: (Num a, CharParsing m) => m a
integral = foldl' (\x y -> x * 10 + y) 0 . map toNumber <$> some digit <?> "integer"
  where
    toNumber c = fromIntegral (ord c - ord '0')
