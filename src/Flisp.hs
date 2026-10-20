module Flisp (
  Datapath.Datapath(..),
  Memory.Memory,
  ControlUnit.ControlUnit(..),
  Counter.Counter(..),
  Inputs.Inputs(..),
  Flisp,
  flash,
  write,
  output,
  clock,
  clock',
  reset,
  interruptRequest,
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
      datapath = Datapath.reflash newMemory datapath,
      irq = Just Bit.Zero
    }

output :: Flisp -> (Byte.Byte, Byte.Byte)
output (ControlUnit.ControlUnit {
  datapath = (Datapath.Datapath {
    memory
  })
}) =
  let
    fb = case Memory.read memory (Byte.fromInt 0xfb) Bit.One of
      Nothing -> Byte.zero
      (Just fb') -> fb'
    fc = case Memory.read memory (Byte.fromInt 0xfc) Bit.One of
      Nothing -> Byte.zero
      (Just fc') -> fc'
  in
    (fb, fc)

clock :: Flisp -> Flisp
clock prev = ControlUnit.clock prev (Byte.zero, Byte.zero)

clock' :: Flisp -> (Byte.Byte, Byte.Byte) -> Flisp
clock' prev externalInputs = ControlUnit.clock prev externalInputs

reset :: Flisp -> Flisp
reset flisp = ControlUnit.reset flisp

interruptRequest :: Flisp -> Flisp
interruptRequest flisp = ControlUnit.interruptRequest flisp

controlSignals :: Flisp -> Instructions.ControlSignals
controlSignals flisp = ControlUnit.controlSignals flisp
