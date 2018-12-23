module Lib
  ( day_1_1
  , day_1_2
  , getNumbers
  ) where

import qualified Data.Set as Set

-- import Debug.Trace (trace)
-- import Text.Printf (printf)

day_1_1 :: [Int] -> Int
day_1_1 = sum

day_1_2 :: [Int] -> Int
day_1_2 ns = go 0 (Set.singleton 0) (cycle ns)
  where
    -- go cur set (x:xs) | trace (printf "XXX cur=%d x=%d %s" cur x (show set)) False = undefined
    go cur _ []       = cur
    go cur set (x:xs) =
        let new = cur + x
        in if Set.member new set
           then new
           else go new (Set.insert new set) xs

getNumbers :: IO [Int]
getNumbers = map dropPlus . lines <$> getContents
  where
    dropPlus ('+':s) = read s
    dropPlus s       = read s
