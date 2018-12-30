module Day3
  ( Claim(..)
  , ClaimId(..)
  , applyClaim
  , countOverclaimed
  , parseClaim
  , state0
  ) where

import           Data.Array.Unboxed
  ( UArray
  , accum
  , elems
  , listArray
  , indices
  , ixmap
  )
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word32)

newtype ClaimId = ClaimId Int
  deriving (Eq, Ord, Show)

-- | @Claim i x y w h@ is a rectangular area of fabric.
--
-- @i@ - claim ID;
-- @x@ - the number of inches between the left edge of the fabric and
--       the left edge of the rectangle;
-- @y@ - the number of inches between the top edge of the fabric and
--       the top edge of the rectangle;
-- @w@ - the width of the rectangle in inches;
-- @h@ - the height of the rectangle in inches.
data Claim = Claim ClaimId Word32 Word32 Word32 Word32
  deriving (Eq, Show)

data Square
  = Unclaimed
  | Claimed ClaimId
  | Overclaimed
  deriving (Eq, Show)

instance Semigroup Square
  where
    Unclaimed <> Unclaimed = Unclaimed
    Unclaimed <> Claimed i = Claimed i
    Claimed i <> Unclaimed = Claimed i
    _ <> _ = Overclaimed

-- We cannot use UArray with Square. We'll be unboxing Square to Int.

toSquare :: Int -> Square
toSquare x | x == 0    = Unclaimed
           | x > 0     = Claimed (ClaimId x)
           | otherwise = Overclaimed

fromSquare :: Square -> Int
fromSquare Unclaimed = 0
fromSquare (Claimed (ClaimId x)) = if x > 0
                                   then x
                                   else error "fromSquare: Invalid argument"
fromSquare Overclaimed = -1

type Fabric = UArray (Word32,Word32) Int

-- Fabric and a set of claims that don't overlap with any other claim.
type ClaimingState = (Fabric, Set ClaimId)

applyClaim :: Claim -> ClaimingState -> ClaimingState
applyClaim (Claim cid x y w h) (fabric, set) =
    let toClaimId :: Square -> Maybe ClaimId
        toClaimId (Claimed i) = Just i
        toClaimId _           = Nothing

        rect = ixmap ((x, y), (x+w-1, y+h-1)) id fabric
        overlappedClaimIds =
            Set.fromList [i | Just i <- toClaimId . toSquare <$> elems rect]
        update e e' = fromSquare (toSquare e <> e')
        fabric' = accum update fabric [(idx, Claimed cid) | idx <- indices rect]
    in ( fabric'
       , if Set.null overlappedClaimIds
         then Set.insert cid set
         else Set.difference set overlappedClaimIds
       )

countOverclaimed :: Fabric -> Word32
countOverclaimed =
    let isOverclaimed e = toSquare e == Overclaimed
    in fromIntegral . length . filter isOverclaimed . elems

parseClaim :: String -> Claim
parseClaim str = Claim (ClaimId $ read i) (read x) (read y) (read w) (read h)
  where
    -- XXX This is ugly and fragile. Use proper parsing library.
    (i, (' ':'@':' ':str1)) = break (== ' ') $ drop 1 str
    (x, (',':str2)) = break (== ',') str1
    (y, (':':' ':str3)) = break (== ':') str2
    (w, ('x':h)) = break (== 'x') str3

state0 :: ClaimingState
state0 = ( listArray ((0,0), (999,999)) (repeat $ fromSquare Unclaimed)
         , Set.empty
         )
