module Lib
  (
  -- Day 1
    d011
  , d012
  , getNumbers
  -- Day 2
  , d021_checksum
  , d021_count
  , d022
  , d022_single_diff
  -- Day 3
  , Claim(..)
  , d031_applyClaim
  , d031_countOverClaims
  , d031_parseClaim
  , d031_unclaimedFabric
  ) where

import           Data.Array.Unboxed (UArray, accum, elems, listArray)
import           Data.Foldable (foldl')
import qualified Data.Map as Map
import           Data.Monoid (First(First), getFirst, mconcat)
import qualified Data.Set as Set
import           Data.Word (Word32)

-- import Debug.Trace (trace)
-- import Text.Printf (printf)

----------------------------------------------------------------------
-- Day 1

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

----------------------------------------------------------------------
-- Day 2

-- | If two words differ by exactly one character at the same position,
-- return its index.
d022_single_diff :: Eq a => [a] -> [a] -> Maybe Int
d022_single_diff xs ys = go Nothing $ zip3 [0..] xs ys
  where
    go mi [] = mi
    go mi ((_, x, y):rest) | x == y = go mi rest        -- no difference
    go Nothing ((i, _, _):rest)     = go (Just i) rest  -- 1st difference
    go (Just _) _                   = Nothing           -- 2nd difference

-- | Try to find a pair of words that differ by exactly one character
-- and return their common letters.
d022 :: Eq a => [[a]] -> Maybe [a]
d022 [] = Nothing
d022 (w:ws) = case getFirst . mconcat $ map (First . common w) ws of
    Nothing -> d022 ws
    result  -> result
  where
    common xs ys = case d022_single_diff xs ys of
        Nothing -> Nothing
        Just i -> Just $ take i xs ++ drop (i+1) xs

----------------------------------------------------------------------
-- Day 3

-- | @Claim x y w h@ is a rectangular area of fabric.
--
-- @x@ - the number of inches between the left edge of the fabric and
--       the left edge of the rectangle;
-- @y@ - the number of inches between the top edge of the fabric and
--       the top edge of the rectangle;
-- @w@ - the width of the rectangle in inches;
-- @h@ - the height of the rectangle in inches.
data Claim = Claim Word32 Word32 Word32 Word32
  deriving (Eq, Show)

type Fabric = UArray (Word32,Word32) Word32

d031_unclaimedFabric :: Fabric
d031_unclaimedFabric = listArray ((0,0), (999,999)) (repeat 0)

d031_applyClaim :: Claim -> Fabric -> Fabric
d031_applyClaim (Claim x y w h) fabric =
    let rect = [((a, b), undefined) | a <- [x..x+w-1], b <- [y..y+h-1]]
    in accum (const . (+1)) fabric rect

d031_countOverClaims :: Fabric -> Word32
d031_countOverClaims = fromIntegral . length . filter (> 1) . elems

d031_parseClaim :: String -> Claim
d031_parseClaim str = Claim (read x) (read y) (read w) (read h)
  where
    -- XXX This is ugly and fragile. Use proper parsing library.
    (x, (',':str1)) = break (== ',') . drop 2 $ dropWhile (/= '@') str
    (y, (':':' ':str2)) = break (== ':') str1
    (w, ('x':h)) = break (== 'x') str2
