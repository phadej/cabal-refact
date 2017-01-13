{-# LANGUAGE OverloadedStrings #-}
module Distribution.Refact.Parser (
    parseFields,
    ) where

import Prelude ()
import Distribution.Refact.Internal.Prelude
import Distribution.Refact.Tools.Trifecta
import Distribution.Refact.Types.Pos
import Distribution.Refact.Types.Structure
import Distribution.Refact.Types.Version

import Data.Char           (isLetter)
import Text.Trifecta.Delta (Delta (..), HasDelta (..), column)

-- | Parse 'Field's
parseFields :: FilePath -> Text -> Result [Field SrcSpan]
parseFields = runIParser cabalFileParser

cabalFileParser :: (DeltaParsing m, IndentationParsing m) => m [Field SrcSpan]
cabalFileParser = fieldsParser <* eof

fieldsParser :: (DeltaParsing m, IndentationParsing m) => m [Field SrcSpan]
fieldsParser = many $
    absoluteIndentation fieldParser <|> localIndentation Any commentParser

fieldParser :: (DeltaParsing m, IndentationParsing m) => m (Field SrcSpan)
fieldParser = do
    name <- nameParser
    fieldParser' name <|> sectionParser name

fieldParser'
    :: (DeltaParsing m, IndentationParsing m)
    => Name SrcSpan -> m (Field SrcSpan)
fieldParser' name = Field name
    <$> srcSpanParser const (char ':')
    <*  spaces'
    <*> fieldValueParser (name ^. nameText)

fieldValueParser
    :: (DeltaParsing m, IndentationParsing m)
    => Text -> m (FieldValue SrcSpan)
fieldValueParser name = fromMaybe fieldLinesParser $ x ^? ix name
  where
    x = toMapOf (folded . ifolded)
        [ ("x-revision", fieldNumberParser)
        , ("version", fieldVersionParser)
        ]

fieldLinesParser
    :: (DeltaParsing m, IndentationParsing m)
    => m (FieldValue SrcSpan)
fieldLinesParser = FieldLines <$> fieldlines
  where
    -- little hacky: we parse also all-spaces lines, but later filter them out
    fieldlines = filter (not . fieldLineNull) <$> localIndentation Gt (many l)
    l = srcSpanParser (\s -> FieldLine s . view packed) (many $ satisfy (/= '\n')) <* nl

fieldNumberParser
    :: (DeltaParsing m, IndentationParsing m)
    => m (FieldValue SrcSpan)
fieldNumberParser =
    spaces' *> optional nl *>
    srcSpanParser FieldNumber integral <* nl

fieldVersionParser
    :: (DeltaParsing m, IndentationParsing m)
    => m (FieldValue SrcSpan)
fieldVersionParser =
    spaces' *> optional nl *>
    srcSpanParser FieldVersion versionParser <* nl

sectionParser :: (DeltaParsing m, IndentationParsing m) => Name SrcSpan -> m (Field SrcSpan)
sectionParser name =
    Section name <$> sectionArgs <*> localIndentation Gt fieldsParser
  where
    sectionArgs = view packed <$> manyTill (satisfy (/= ':')) nl

commentParser :: (DeltaParsing m, IndentationParsing m) => m (Field SrcSpan)
commentParser = (srcSpanParser Comment $ view packed <$> p) <* nl
  where
    p = (<>) <$> string "--" <*> many (satisfy (/= '\n'))

nameParser :: (DeltaParsing m) => m (Name SrcSpan)
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
