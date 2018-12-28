module Main where

import qualified Lib

main :: IO ()
main = print . Lib.d022 =<< lines <$> getContents
