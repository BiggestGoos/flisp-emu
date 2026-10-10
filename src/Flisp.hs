module Flisp (
  Datapath.Datapath(..),
  ControlUnit.ControlUnit(..),
  Counter.Counter(..),
  Inputs.Inputs(..),
  Flisp,
  flash,
  clock,
  reset,
  controlSignals,
) where

import qualified Types.Byte as Byte
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

clock :: Flisp -> Flisp
clock prev = ControlUnit.clock prev

reset :: Flisp -> Flisp
reset flisp = ControlUnit.reset flisp

controlSignals :: Flisp -> Instructions.ControlSignals
controlSignals flisp = ControlUnit.controlSignals flisp
