module Main where

import qualified Day3 as D

import Data.Foldable (foldl')

main :: IO ()
main = print . D.countOverClaims . foldl' (flip D.applyClaim) D.unclaimedFabric . map D.parseClaim =<< lines <$> getContents
