{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Edit distance with a witness.
module Distribution.Refact.Tools.Edit.Algorithm where

import Prelude ()
import Distribution.Refact.Internal.Prelude

data Edit a
    = Take a
    | Drop a
    | Add a
  deriving (Eq, Show)

-- | /Reverse/ of the 'diff'.
--
-- >>> patch [Take "a"] ["a"]
-- Just ["a"]
--
patch :: Eq a => [Edit a] -> [a] -> Maybe [a]
patch []            []                   = Just []
patch []            (_ : _)              = Nothing
patch (Add a  : es) xs                   = (a :) <$> patch es xs
patch (Take _ : _)  []                   = Nothing
patch (Take a : es) (x : xs) | a == x    = (a :) <$> patch es xs
                             | otherwise = Nothing
patch (Drop _ : _)  []                   = Nothing
patch (Drop a : es) (x : xs) | a == x    = patch es xs
                             | otherwise = Nothing

-- | Sort edits so there are 'Drop's before 'Add's.
--
-- /TODO:/ should there be 'Add's before 'Take's?
--
-- >>> improve $ diff "ffoo" "bbar"
-- [Drop 'b',Drop 'b',Drop 'a',Drop 'r',Add 'f',Add 'f',Add 'o',Add 'o']
--
improve :: [Edit a] -> [Edit a]
improve = reverse . foldl i []
  where
    i :: [Edit a] -> Edit a -> [Edit a]
    i [] x = [x]
    i (Add y : ys) x@(Drop _) = Add y : i ys x
    i xs x = x : xs

-- | Group edits into hunks.
hunks :: Int -> [Edit a] -> [[Edit a]]
hunks lim = g . reverse . f 0 . reverse . f 0 . map (False,)
  where
    f _ []                 = []
    f n ((b, Take t) : xs) = (b || n > 0, Take t) : f (n - 1) xs
    f _ ((_, Drop t) : xs) = (True, Drop t) : f lim xs
    f _ ((_, Add  t) : xs) = (True, Add t ) : f lim xs

    g [] = []
    g xs@((True, _) : _) = let (a, b) = span fst xs in map snd a : g b
    g ((False, _) : xs)  = g xs

-- | Diff. Calculates the minimum edit distance, returns a witness.
--
-- >>> diff "ffoo" "bboo"
-- [Add 'f',Drop 'b',Add 'f',Drop 'b',Take 'o',Take 'o']
--
diff :: forall a. Eq a => [a] -> [a] -> [Edit a]
diff xs ys = appEndo (snd $ lev xn yn) []
  where
    lev :: Int -> Int -> (Int, Endo [Edit a])
    lev i j = memo ^?! ix i . ix j

    memo :: Vector (Vector (Int, Endo [Edit a]))
    memo = [ [ lev' i j | j <- [0..yn] ] ^. vector | i <- [0..xn] ] ^. vector

    lev' :: Int -> Int -> (Int, Endo [Edit a])
    lev' i j | i <= 0 && j <= 0 = (0, Endo id)
    lev' i j | i <= 0 = (j, fromList (Drop <$> take j ys))
    lev' i j | j <= 0 = (i, fromList (Add <$> take i xs))
    lev' i j = head $ sortOn fst $
        [ bimap (+1) (sn $ Add x) $ lev i' j
        , bimap (+1) (sn $ Drop y) $ lev i j'
        , t
        ]
      where
        i' = i - 1
        j' = j - 1
        x = vxs ^?! ix i'
        y = vys ^?! ix j'
        t | x == y =
            bimap id (sn (Take x)) $ lev i' j'
          | otherwise            =
            bimap (+1) (sn (Drop y) . sn (Add x)) $ lev i' j'

    vxs = xs ^. vector
    vys = ys ^. vector

    xn = length vxs
    yn = length vys

    sn :: forall b. b -> Endo [b] -> Endo [b]
    sn b = (<> Endo (b :))

    fromList :: forall b. [b] -> Endo [b]
    fromList bs = Endo (bs ++)

-- | Trivial diff
--
-- >>> length (trivialDiff "foo" "boo")
-- 6
--
-- >>> patch (trivialDiff "foo" "boo") "foo"
-- Just "boo"
--
trivialDiff :: [a] -> [a] -> [Edit a]
trivialDiff xs ys = map Drop xs ++ map Add ys
