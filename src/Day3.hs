module Day3
  ( Claim(..)
  , applyClaim
  , countOverClaims
  , parseClaim
  , unclaimedFabric
  ) where

import Data.Array.Unboxed (UArray, accum, elems, listArray)
import Data.Word (Word32)

-- | @Claim x y w h@ is a rectangular area of fabric.
--
-- @x@ - the number of inches between the left edge of the fabric and
--       the left edge of the rectangle;
-- @y@ - the number of inches between the top edge of the fabric and
--       the top edge of the rectangle;
-- @w@ - the width of the rectangle in inches;
-- @h@ - the height of the rectangle in inches.
data Claim = Claim Word32 Word32 Word32 Word32
  deriving (Eq, Show)

type Fabric = UArray (Word32,Word32) Word32

unclaimedFabric :: Fabric
unclaimedFabric = listArray ((0,0), (999,999)) (repeat 0)

applyClaim :: Claim -> Fabric -> Fabric
applyClaim (Claim x y w h) fabric =
    let rect = [((a, b), undefined) | a <- [x..x+w-1], b <- [y..y+h-1]]
    in accum (const . (+1)) fabric rect

countOverClaims :: Fabric -> Word32
countOverClaims = fromIntegral . length . filter (> 1) . elems

parseClaim :: String -> Claim
parseClaim str = Claim (read x) (read y) (read w) (read h)
  where
    -- XXX This is ugly and fragile. Use proper parsing library.
    (x, (',':str1)) = break (== ',') . drop 2 $ dropWhile (/= '@') str
    (y, (':':' ':str2)) = break (== ':') str1
    (w, ('x':h)) = break (== 'x') str2
