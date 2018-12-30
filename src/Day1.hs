module Day1
  ( d011
  , d012
  , getNumbers
  ) where

import qualified Data.Set as Set

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
    go cur _ []       = cur
    go cur set (x:xs) =
        let new = cur + x
        in if Set.member new set
           then new
           else go new (Set.insert new set) xs
