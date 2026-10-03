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

clock :: ControlUnit -> ControlUnit
clock (ControlUnit {
  counter,
  datapath = datapath@(Datapath.Datapath {
    reg_i,
    reg_cc
  })
}) =
  let
    (nf, inputs) = Instructions.create counter reg_i reg_cc
  in
    ControlUnit {
      counter = Counter.clock counter nf,
      datapath = Datapath.clock datapath inputs
    }
