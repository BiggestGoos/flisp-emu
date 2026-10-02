module Types.Register where

import Types.Byte as Byte
import Types.Bit as Bit
import Types.Bus as Bus

type Register = Byte

read :: Register -> Bit -> Bus.Bus
read _ Bit.Zero = Nothing
read register Bit.One = Just (register :: Byte)

write :: Register -> Byte -> Bit -> Register
write register _ Bit.Zero = register
write _ input Bit.One = input :: Register
