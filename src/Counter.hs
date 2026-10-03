module Counter where

import qualified Types.Bit as Bit

data Counter = Counter { q3, q2, q1, q0 :: Bit.Bit } deriving (Eq, Show)

reset :: Counter
reset = Counter Bit.Zero Bit.Zero Bit.Zero Bit.Zero

clock :: Counter -> Bit.Bit -> Counter
clock (Counter q3 q2 q1 q0) nf =
  let
    value = Bit.bitsToInt [q3, q2, q1, q0]
    newFetchPhase = 3
    newBits =
      if value >= newFetchPhase && nf == Bit.One
      then Bit.bitsFromInt newFetchPhase 4
      else Bit.bitsFromInt (value + 1) 4
    nq3 = newBits !! 0
    nq2 = newBits !! 1
    nq1 = newBits !! 2
    nq0 = newBits !! 3
  in
    Counter nq3 nq2 nq1 nq0
