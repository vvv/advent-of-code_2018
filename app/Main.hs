module Main where

import qualified Lib

main :: IO ()
main = lines <$> getContents >>=
    print . Lib.d021_checksum . map Lib.d021_count
