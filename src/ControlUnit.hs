module ControlUnit where

import qualified Datapath
import qualified Counter
import qualified Instructions
import qualified Types.Memory as Memory
import qualified Types.Byte as Byte
import qualified Types.Bit as Bit

data ControlUnit = ControlUnit { counter :: Counter.Counter, datapath :: Datapath.Datapath, irq :: Maybe Bit.Bit } deriving (Show)

flash :: Memory.Memory -> ControlUnit
flash memory = ControlUnit {
  counter = Counter.reset,
  datapath = Datapath.flash memory,
  irq = Just Bit.Zero
}

controlSignals :: ControlUnit -> Instructions.ControlSignals
controlSignals (ControlUnit {
  counter,
  datapath = (Datapath.Datapath {
    reg_i,
    reg_cc
  }),
  irq
}) = Instructions.create counter reg_i reg_cc irq

clock :: ControlUnit -> (Byte.Byte, Byte.Byte) -> ControlUnit
clock controlUnit@(ControlUnit {
  counter,
  datapath
})
  externalInputs =
  let
    (nf, inputs, newIrq) = controlSignals controlUnit
  in
    ControlUnit {
      counter = Counter.clock counter nf,
      datapath = Datapath.clock datapath inputs externalInputs,
      irq = newIrq
    }

reset :: ControlUnit -> ControlUnit
reset (ControlUnit {
  datapath
}) = ControlUnit {
  counter = Counter.reset,
  datapath = datapath,
  irq = Just Bit.Zero
}

interruptRequest :: ControlUnit -> ControlUnit
interruptRequest (ControlUnit {
  counter,
  datapath
}) = ControlUnit {
  counter = counter,
  datapath = datapath,
  irq = Just Bit.One
}
