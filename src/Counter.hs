module Counter where

import qualified Types.Bit as Bit

data Counter = Counter { q3, q2, q1, q0 :: Bit.Bit } deriving (Ord, Eq, Show)

bits :: Counter -> [Bit.Bit]
bits (Counter q3 q2 q1 q0) = [q3, q2, q1, q0]

unbits :: [Bit.Bit] -> Counter
unbits (q3:q2:q1:q0:_) = Counter q3 q2 q1 q0
unbits _ = error "Only lists of bits with length 4 or more can be converted to a counter!"

reset :: Counter
reset = Counter Bit.Zero Bit.Zero Bit.Zero Bit.Zero

clock :: Counter -> Bit.Bit -> Counter
clock counter nf =
  let
    newFetchPhase = Counter Bit.Zero Bit.Zero Bit.One Bit.One
  in
    if counter >= newFetchPhase && nf == Bit.One
    then newFetchPhase
    else unbits $ fst $ Bit.fullAdderBits (bits counter) (bits Counter.reset) Bit.One
