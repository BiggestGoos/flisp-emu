module Types.Inputs where

import Types.Bit

data Inputs = Inputs {
  -- Load enable
  ld_a,
  ld_t,
  ld_x,
  ld_y,
  ld_pc,
  ld_sp,
  ld_ta,
  ld_r,
  ld_cc,
  ld_i,
  -- Open enable
  oe_a,
  oe_x,
  oe_y,
  oe_pc,
  oe_sp,
  oe_r,
  oe_cc,
  -- Clear
  clr_t,
  -- Increase
  inc_pc,
  inc_sp,
  -- Decrease
  dec_sp,
  -- ALU Function
  f3,
  f2,
  f1,
  f0,
  -- Memory
  mr,
  mw,
  -- Multiplexers
  g14,
  g13,
  g12,
  g11,
  g10,
  g9,
  g8,
  g7,
  g6,
  g5,
  g4,
  g3,
  g2,
  g1,
  g0 :: Bit
} deriving (Show)

zero :: Inputs
zero = Inputs Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero
