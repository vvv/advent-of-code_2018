import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib (day_1_1, day_1_2)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "day-1_1" $ do
        let f = day_1_1
        f [1, 1, 1] @?= 3
        f [1, 1, -2] @?= 0
        f [-1, -2, -3] @?= -6
  , testCase "day-1_2" $ do
        let f = day_1_2
        f [1, -1] @?= 0
        f [3, 3, 4, -2, -4] @?= 10
        f [-6, 3, 8, 5, -6] @?= 5
        f [7, 7, -2, -7, -4] @?= 14
  ]
