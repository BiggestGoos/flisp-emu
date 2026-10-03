module MyLib where

import qualified Types.Byte as Byte
import qualified Types.Memory as Memory
import qualified ControlUnit

type FLISP = ControlUnit.ControlUnit

flash :: [Int] -> FLISP
flash input =
  let
    memory = Memory.fromList (map (\ int -> Byte.fromInt int) input)
  in
    ControlUnit.flash memory

clock :: FLISP -> FLISP
clock prev = ControlUnit.clock prev
