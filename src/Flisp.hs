module Flisp where

import qualified Types.Byte as Byte
import qualified Types.Memory as Memory
import qualified ControlUnit

type Flisp = ControlUnit.ControlUnit

flash :: [Int] -> Flisp
flash input =
  let
    memory = Memory.fromList (map (\ int -> Byte.fromInt int) input)
  in
    ControlUnit.flash memory

clock :: Flisp -> Flisp
clock prev = ControlUnit.clock prev
