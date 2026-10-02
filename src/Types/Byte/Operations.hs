module Types.Byte.Operations where

import Types.Bit
import Types.Byte.Type

add :: Byte -> Byte -> Carry -> (Byte, Carry)
add x y cin =
  let
    (bits', carry) = fullAdderBits (bits x) (bits y) cin
  in
    (unbits bits', carry)

firstComplement :: Byte -> Byte
firstComplement byte = unbits $ notb $ bits byte

data Direction = Left | Right deriving (Eq)
logicalShift :: Byte -> Bit -> Direction -> (Byte, Bit)
logicalShift (Byte a b c d e f g h) shiftIn Types.Byte.Operations.Left  = (Byte b c d e f g h shiftIn, a)
logicalShift (Byte a b c d e f g h) shiftIn Types.Byte.Operations.Right = (Byte shiftIn a b c d e f g, h)

arithmeticShift :: Byte -> (Byte, Bit)
arithmeticShift (Byte a b c d e f g h) = (Byte a a b c d e f g, h)
