module Lib
  ( d011
  , d012
  , d021_count
  , d021_checksum
  , getNumbers
  ) where

import           Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set

-- import Debug.Trace (trace)
-- import Text.Printf (printf)

getNumbers :: IO [Int]
getNumbers = map dropPlus . lines <$> getContents
  where
    dropPlus ('+':s) = read s
    dropPlus s       = read s

d011 :: [Int] -> Int
d011 = sum

d012 :: [Int] -> Int
d012 ns = go 0 (Set.singleton 0) (cycle ns)
  where
    -- go cur set (x:xs) | trace (printf "XXX cur=%d x=%d %s" cur x (show set)) False = undefined
    go cur _ []       = cur
    go cur set (x:xs) =
        let new = cur + x
        in if Set.member new set
           then new
           else go new (Set.insert new set) xs

-- | Given a word, return a pair @(p, q)@, where
-- @p@ is True iff the word contains exactly two of any letter and
-- @q@ is True iff the word contains exactly three of any letter.
d021_count :: (Foldable t, Eq a, Ord a) => t a -> (Bool, Bool)
d021_count = go (False, False) . Map.elems . letterCounts
  where
    addLetter :: (Ord k, Num a, Enum a) => k -> Map.Map k a -> Map.Map k a
    addLetter = Map.alter (Just . maybe 1 succ)

    letterCounts :: (Foldable t, Ord k, Num a, Enum a) => t k -> Map.Map k a
    letterCounts = foldl' (flip addLetter) Map.empty

    go :: (Bool, Bool) -> [Integer] -> (Bool, Bool)
    go (True, True) _ = (True, True)
    go z []           = z
    go z (2:xs)       = go (True, snd z) xs
    go z (3:xs)       = go (fst z, True) xs
    go z (_:xs)       = go z xs

d021_counts :: Foldable t => t (Bool, Bool) -> (Integer, Integer)
d021_counts = foldr f (0, 0)
  where
    f (p, q) (x, y) = (succIf p x, succIf q y)
    succIf p x = if p then succ x else x

d021_checksum :: Foldable t => t (Bool, Bool) -> Integer
d021_checksum ps =
    let (x, y) = d021_counts ps
    in x*y
