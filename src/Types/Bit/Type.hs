module Types.Bit.Type where

import Prelude (Ord, Eq, Show, Int, Bool(..), show, error, (++), ($), map, foldl, zip, length, (==), (-))
import Data.Bits

data Bit = Zero | One deriving (Ord, Eq)

instance Show Bit where
  show Zero = "0"
  show One  = "1"

-- Int <-> Bit
toInt :: Bit -> Int
toInt Zero = 0
toInt One  = 1

fromInt :: Int -> Bit
fromInt 0 = Zero
fromInt 1 = One
fromInt x = error $ show x ++ " can't be interpreted as a bit!"

-- Bool <-> Bit
toBool :: Bit -> Bool
toBool Zero = False
toBool One  = True

fromBool :: Bool -> Bit
fromBool False = Zero
fromBool True  = One

-- Int <-> [Bit]

bitsFromInt :: Int -> Int -> [Bit]
bitsFromInt int n =
  let
    bits' = map (\ i -> fromBool $ Data.Bits.testBit int i) [n-1,n-2..0]
  in bits'

bitsToInt :: [Bit] -> Int
bitsToInt bits =
  let
    n = length bits
    -- Indexed bits
    bits' = zip bits [n-1,n-2..0]
  in
    foldl (\ acc (b, i) ->
      if b == One
      then Data.Bits.setBit acc i
      else acc) 0 bits'
