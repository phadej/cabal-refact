{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Refact.Tools.Pretty where

import Prelude ()
import Distribution.Refact.Internal.Prelude

import Distribution.Refact.Types.Pos

import Control.Monad.State.Strict (State, execState)

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as B

data WS = WS !Pos !B.Builder

wsPos :: Lens' WS Pos
wsPos f (WS p b) = flip WS b <$> f p

wsBuilder :: Lens' WS B.Builder
wsBuilder f (WS p b) = WS p <$> f b

newtype W a = W { unW :: State WS a }
  deriving (Functor, Applicative, Monad)

appendBuilder :: B.Builder -> State WS ()
appendBuilder b = wsBuilder %= (<> b)

prettyNewline :: W ()
prettyNewline = W $ do
    wsPos . posLine += 1
    appendBuilder $ B.singleton '\n'

-- | Assumes there is no over UTF16 characters, neither newlines in 'Text'.
prettyTextAt :: Pos -> Text -> W ()
prettyTextAt p t = W $ do
    p'@(Pos l' c') <- use wsPos
    let Pos l c =  max p p'

    let (ls, cs) = case compare l l' of
            EQ -> (0, c - c')
            LT -> (0, 0)
            GT -> (l - l', c)
    let b1 = B.fromString (replicate ls '\n')
    let b2 = b1 <> B.fromString (replicate cs ' ')
    let b3 = b2 <> B.fromText t'

    wsPos .= Pos l (c + T.length t')
    appendBuilder b3
  where
    t' = T.filter (/= '\n') t

prettyText :: Text -> W ()
prettyText = prettyTextAt (Pos 0 0) -- abusing implementation

execW :: W a -> TL.Text
execW (W s) = let WS _ b = execState s (WS (Pos 0 0) mempty) in B.toLazyText b
