module ALU
(
  Function(..),
  functionFromInt,
  Flags(..),
  alu,
)where

import Types.Byte as Byte
import Types.Bit as Bit
import qualified Data.List.NonEmpty as NE

data Function = Function { f3 :: Bit, f2 :: Bit, f1 :: Bit, f0 :: Bit } deriving (Show)
functionFromInt :: Int -> Function
functionFromInt int =
  let
    bits' = Bit.bitsFromInt int 4
    n = bits' !! 0
    z = bits' !! 1
    v = bits' !! 2
    c = bits' !! 3
  in
    Function n z v c
data Flags = Flags { n :: Bit, z :: Bit, v :: Bit, c :: Bit } deriving (Show)
zeroFlags :: Flags
zeroFlags = Flags Zero Zero Zero Zero

carryFlag :: Bit -> Flags
carryFlag bit = Flags Zero Zero Zero bit

testN :: (Byte, Flags) -> (Byte, Flags)
testN (byte, (Flags _ z v c)) =
  let
    n NE.:| _ = (NE.fromList $ bits byte)
  in
    (byte, (Flags n z v c))

testZ :: (Byte, Flags) -> (Byte, Flags)
testZ (byte, (Flags n _ v c)) =
  let
    z = Bit.norn $ bits byte
  in
    (byte, (Flags n z v c))

-- Tests if an addition between d and e resulted in an overflow
testV1 :: Byte -> Byte -> (Byte, Flags) -> (Byte, Flags)
testV1 (Byte d7 _ _ _ _ _ _ _) (Byte e7 _ _ _ _ _ _ _) (byte@(Byte u7 _ _ _ _ _ _ _), (Flags n z _ c)) =
  let
    v = (Bit.not u7 `Bit.and` d7 `Bit.and` e7) `Bit.or` (u7 `Bit.and` (Bit.not d7) `Bit.and` (Bit.not e7))
  in
    (byte, (Flags n z v c))

-- Tests if a bitshift in a given direction resulted in an overflow
testV2 :: Byte -> Bit -> Byte.Direction -> (Byte, Flags) -> (Byte, Flags)
testV2 (Byte d7 d6 _ _ _ _ _ _) cin direction (byte, (Flags n z _ c)) =
  let
    v = case direction of
      Byte.Left  -> d7 `Bit.xor` d6
      Byte.Right -> d7 `Bit.xor` cin
  in
    (byte, (Flags n z v c))

-- Tests V for d == 0x80 and C for d /= 0x00
testVC :: Byte -> (Byte, Flags) -> (Byte, Flags)
testVC d (byte, (Flags n z _ _)) =
  let
    v = Bit.fromBool $ d == (Byte.fromInt 0x80)
    c = Bit.fromBool $ d /= zero
  in
    (byte, (Flags n z v c))

-- alu <function> <d> <e> <cin> = (<result>, <flags>)
alu :: Function -> Byte -> Byte -> Carry -> (Byte, Flags)
alu (Function Zero Zero Zero Zero) _ _ _   =                                                                                       testZ                             (zero                                 , zeroFlags                )
alu (Function Zero Zero Zero One ) _ _ _   =                                                                               testN                                     (Byte.fromInt 0xfd                    , zeroFlags                )
alu (Function Zero Zero One  Zero) _ _ _   =                                                                               testN                                     (Byte.fromInt 0xfe                    , zeroFlags                )
alu (Function Zero Zero One  One ) _ _ _   =                                                                               testN                                     (Byte.fromInt 0xff                    , zeroFlags                )
alu (Function Zero One  Zero Zero) _ e _   =                                                                               testN $ testZ                             (e                                    , zeroFlags                )
alu (Function Zero One  Zero One ) d _ cin = let d' = firstComplement d;                                                in testN $ testZ $ (testVC d)                (fst $ add d' zero cin                , zeroFlags                )
alu (Function Zero One  One  Zero) d e _   =                                                                               testN $ testZ                             (unbits $ (bits d) `Bit.orb`  (bits e), zeroFlags                )
alu (Function Zero One  One  One ) d e _   =                                                                               testN $ testZ                             (unbits $ (bits d) `Bit.andb` (bits e), zeroFlags                )
alu (Function One  Zero Zero Zero) d e _   =                                                                               testN $ testZ                             (unbits $ (bits d) `Bit.xorb` (bits e), zeroFlags                )
alu (Function One  Zero Zero One ) d _ cin = let d' = d; e' = zero;              (result, carry) = Byte.add d' e' cin;  in testN $ testZ $ (testV1 d' e')            (result                               , carryFlag carry          )
alu (Function One  Zero One  Zero) d _ _   = let d' = d; e' = full;              (result, carry) = Byte.add d' e' Zero; in testN $ testZ $ (testV1 d' e')            (result                               , carryFlag carry          )
alu (Function One  Zero One  One ) d e cin = let d' = d; e' = e;                 (result, carry) = Byte.add d' e' cin;  in testN $ testZ $ (testV1 d' e')            (result                               , carryFlag carry          )
alu (Function One  One  Zero Zero) d e cin = let d' = d; e' = firstComplement e; (result, carry) = Byte.add d' e' cin;  in testN $ testZ $ (testV1 d' e')            (result                               , carryFlag $ Bit.not carry)
alu (Function One  One  Zero One ) d _ cin = let (result, shiftOut) = logicalShift d cin Byte.Left;                     in testN $ testZ $ (testV2 d cin Byte.Left)  (result                               , carryFlag shiftOut       )
alu (Function One  One  One  Zero) d _ cin = let (result, shiftOut) = logicalShift d cin Byte.Right;                    in testN $ testZ $ (testV2 d cin Byte.Right) (result                               , carryFlag shiftOut       )
alu (Function One  One  One  One ) d _ _   = let (result, shiftOut) = arithmeticShift d;                                in testN $ testZ                             (result                               , carryFlag shiftOut       )
