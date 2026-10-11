module Flisp (
  Datapath.Datapath(..),
  Memory.Memory,
  ControlUnit.ControlUnit(..),
  Counter.Counter(..),
  Inputs.Inputs(..),
  Flisp,
  flash,
  write,
  clock,
  reset,
  controlSignals,
) where

import qualified Types.Byte as Byte
import qualified Types.Bit as Bit
import qualified Types.Memory as Memory
import qualified Types.Inputs as Inputs
import qualified Datapath
import qualified ControlUnit
import qualified Counter
import qualified Instructions

type Flisp = ControlUnit.ControlUnit

flash :: [Int] -> Flisp
flash input =
  let
    memory = Memory.fromList (map (\ int -> Byte.fromInt int) input)
  in
    ControlUnit.flash memory

write :: Int -> Int -> Flisp -> Flisp
write input index (ControlUnit.ControlUnit {
  counter,
  datapath = datapath@(Datapath.Datapath {
    memory
  })
}) =
  let
    newMemory = Memory.write memory (Byte.fromInt index) Bit.One (Byte.fromInt input)
  in
    ControlUnit.ControlUnit {
      counter = counter,
      datapath = Datapath.reflash newMemory datapath
    }

clock :: Flisp -> Flisp
clock prev = ControlUnit.clock prev

reset :: Flisp -> Flisp
reset flisp = ControlUnit.reset flisp

controlSignals :: Flisp -> Instructions.ControlSignals
controlSignals flisp = ControlUnit.controlSignals flisp
