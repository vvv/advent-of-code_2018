import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@=?))

import qualified Lib

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "day-1_1" $ do
        let f = Lib.d011
        f [1, 1, 1] @?= 3
        f [1, 1, -2] @?= 0
        f [-1, -2, -3] @?= -6
  , testCase "day-1_2" $ do
        let f = Lib.d012
        f [1, -1] @?= 0
        f [3, 3, 4, -2, -4] @?= 10
        f [-6, 3, 8, 5, -6] @?= 5
        f [7, 7, -2, -7, -4] @?= 14
  , testCase "day-2_1" $ do
        let f = Lib.d021_count
        f "abcdef" @?= (False, False)
        f "bababc" @?= (True, True)
        f "abbcde" @?= (True, False)
        f "abcccd" @?= (False, True)
        f "aabcdd" @?= (True, False)
        f "abcdee" @?= (True, False)
        f "ababab" @?= (False, True)
        2 @=? Lib.d021_checksum [(False, False), (True, True), (False, True)]
  , testCase "day-2_2" $ do
        let f = Lib.d022_single_diff
        f "fghij" "fguij" @?= Just 2
        f "abcde" "abcde" @?= Nothing
        f "abcde" "Abcde" @?= Just 0
        f "ab12e" "abcde" @?= Nothing
        (Just "fgij") @=? Lib.d022 [ "abcde"
                                   , "fghij"
                                   , "klmno"
                                   , "pqrst"
                                   , "fguij"
                                   , "axcye"
                                   , "wvxyz"
                                   ]
  ]
