module Distribution.Refact.Parser (
    parseFields,
    ) where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Tools.Trifecta
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Structure

import Data.Char           (isLetter)
import Text.Trifecta.Delta (Delta (..), HasDelta (..), column)

-- | Parse 'Field's
parseFields :: FilePath -> Text -> Result [Field SrcSpan]
parseFields = runIParser cabalFileParser

cabalFileParser :: (CharParsing m, DeltaParsing m, IndentationParsing m) => m [Field SrcSpan]
cabalFileParser = fieldsParser <* eof

fieldsParser :: (CharParsing m, DeltaParsing m, IndentationParsing m) => m [Field SrcSpan]
fieldsParser = many $
    absoluteIndentation fieldParser <|> localIndentation Any commentParser

fieldParser :: (CharParsing m, DeltaParsing m, IndentationParsing m) => m (Field SrcSpan)
fieldParser = do
    name <- nameParser
    fieldParser' name <|> sectionParser name

fieldParser' :: (CharParsing m, DeltaParsing m, IndentationParsing m) => Name SrcSpan -> m (Field SrcSpan)
fieldParser' name =
    Field name <$> srcSpanParser const (char ':') <* spaces' <*> fieldlines
  where
    fieldlines = mk <$> l <*> localIndentation Gt (many l)
    mk h t = filter (not . fieldLineNull) (h : t)
    -- it's important not to use manyTill, otherwise newline will be included in the span
    l = srcSpanParser (\s -> FieldLine s . view packed) (many $ satisfy (/= '\n')) <* nl

sectionParser :: (CharParsing m, DeltaParsing m, IndentationParsing m) => Name SrcSpan -> m (Field SrcSpan)
sectionParser name =
    Section name <$> sectionArgs <*> localIndentation Gt fieldsParser
  where
    sectionArgs = view packed <$> manyTill (satisfy (/= ':')) nl

commentParser :: (CharParsing m, DeltaParsing m, IndentationParsing m) => m (Field SrcSpan)
commentParser = (srcSpanParser Comment $ view packed <$> p) <* nl
  where
    p = (<>) <$> string "--" <*> many (satisfy (/= '\n'))

nameParser :: (CharParsing m, DeltaParsing m) => m (Name SrcSpan)
nameParser = srcSpanParser Name name' <* spaces' <?> "name"
  where
    name' = fmap (view packed) $ (:) <$> hp <*> tp
    hp = satisfy isLetter
    tp = many $ satisfy $ \c -> isLetter c || c == '-'

-------------------------------------------------------------------------------
-- SrcSpan
-------------------------------------------------------------------------------

lineNum :: HasDelta t => t -> Int64
lineNum t = case delta t of
  Columns _ _        -> 0
  Tab _ _ _          -> 0
  Lines l _ _ _      -> l
  Directed _ l _ _ _ -> l

srcSpanParser :: DeltaParsing m => (SrcSpan -> a -> b) -> m a -> m b
srcSpanParser f p = do
    s <- pos
    x <- p
    e <- pos
    spaces'
    pure $ f (SS s e) x
  where

pos :: DeltaParsing m => m Pos
pos = (\d -> Pos (fromIntegral $ lineNum d) (fromIntegral $ column d)) <$> position
