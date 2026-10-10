module ControlUnit where

import qualified Datapath
import qualified Counter
import qualified Instructions
import qualified Types.Memory as Memory

data ControlUnit = ControlUnit { counter :: Counter.Counter, datapath :: Datapath.Datapath } deriving (Show)

flash :: Memory.Memory -> ControlUnit
flash memory = ControlUnit {
  counter = Counter.reset,
  datapath = Datapath.flash memory
}

controlSignals :: ControlUnit -> Instructions.ControlSignals
controlSignals (ControlUnit {
  counter,
  datapath = datapath@(Datapath.Datapath {
    reg_i,
    reg_cc
  })
}) = Instructions.create counter reg_i reg_cc

clock :: ControlUnit -> ControlUnit
clock controlUnit@(ControlUnit {
  counter,
  datapath
}) =
  let
    (nf, inputs) = controlSignals controlUnit
  in
    ControlUnit {
      counter = Counter.clock counter nf,
      datapath = Datapath.clock datapath inputs
    }

reset :: ControlUnit -> ControlUnit
reset (ControlUnit {
  datapath
}) = ControlUnit {
  counter = Counter.reset,
  datapath = datapath
}
