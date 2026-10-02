module Types.Inputs where

import Types.Bit

data Inputs = Inputs {
  -- Load enable
  ld_a   :: Bit,
  ld_t   :: Bit,
  ld_x   :: Bit,
  ld_y   :: Bit,
  ld_pc  :: Bit,
  ld_sp  :: Bit,
  ld_ta  :: Bit,
  ld_r   :: Bit,
  ld_cc  :: Bit,
  ld_i   :: Bit,
  -- Open enable
  oe_a   :: Bit,
  oe_x   :: Bit,
  oe_y   :: Bit,
  oe_pc  :: Bit,
  oe_sp  :: Bit,
  oe_r   :: Bit,
  oe_cc  :: Bit,
  -- Clear
  clr_t  :: Bit,
  -- Increase
  inc_pc :: Bit,
  inc_sp :: Bit,
  -- Decrease
  dec_sp :: Bit,
  -- ALU Function
  f3     :: Bit,
  f2     :: Bit,
  f1     :: Bit,
  f0     :: Bit,
  -- Memory
  mr     :: Bit,
  mw     :: Bit,
  -- Multiplexers
  g14    :: Bit,
  g13    :: Bit,
  g12    :: Bit,
  g11    :: Bit,
  g10    :: Bit,
  g9     :: Bit,
  g8     :: Bit,
  g7     :: Bit,
  g6     :: Bit,
  g5     :: Bit,
  g4     :: Bit,
  g3     :: Bit,
  g2     :: Bit,
  g1     :: Bit,
  g0     :: Bit
} deriving (Show)

zero :: Inputs
zero = Inputs Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero
