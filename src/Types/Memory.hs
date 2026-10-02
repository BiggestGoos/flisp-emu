module Types.Memory where

import Types.Byte as Byte
import Types.Bit as Bit
import Types.Bus as Bus
import qualified Data.Vector.Sized as V

type Address = Byte.Byte
type Memory = V.Vector 256 Byte.Byte

zero :: Memory
zero = V.replicate (Byte.fromInt 0x00) :: Memory

read :: Memory -> Address -> Bit -> Bus.Bus
read _ _ Bit.Zero = Nothing
read memory address Bit.One =
  let
    index = Byte.toInt address
  in
    -- Index can't be outside the range of 0-255
    Just $ V.unsafeIndex memory index

write :: Memory -> Address -> Bit -> Byte -> Memory
write memory _ Bit.Zero _ = memory
write memory address Bit.One input =
  let
    index = Byte.toInt address
  in
    V.unsafeUpd memory [(index, input)]
