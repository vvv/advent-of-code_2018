module Main where

import qualified Day3 as D

import Data.Bifunctor (first)
import Data.Foldable (foldl')

main :: IO ()
main = print . first D.countOverclaimed . foldl' (flip D.applyClaim) D.state0 . map D.parseClaim =<< lines <$> getContents
