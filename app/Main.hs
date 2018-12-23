module Main where

import Lib (day_1_2, getNumbers)

main :: IO ()
main = print =<< day_1_2 <$> getNumbers
