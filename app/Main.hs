module Main where

import qualified Lib

import Data.Foldable (foldl')

main :: IO ()
main = print . Lib.d031_countOverClaims . foldl' (flip Lib.d031_applyClaim) Lib.d031_unclaimedFabric . map Lib.d031_parseClaim =<< lines <$> getContents
