module Types.Inputs.Type where

import Types.Bit

import Types.Inputs.Constants as C

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

create :: [Int] -> Inputs
create inputs =
  let
    test name = if name `elem` inputs then One else Zero
  in
    Inputs {
      ld_a   = test C.ld_a,
      ld_t   = test C.ld_t,
      ld_x   = test C.ld_x,
      ld_y   = test C.ld_y,
      ld_pc  = test C.ld_pc,
      ld_sp  = test C.ld_sp,
      ld_ta  = test C.ld_ta,
      ld_r   = test C.ld_r,
      ld_cc  = test C.ld_cc,
      ld_i   = test C.ld_i,
      oe_a   = test C.oe_a,
      oe_x   = test C.oe_x,
      oe_y   = test C.oe_y,
      oe_pc  = test C.oe_pc,
      oe_sp  = test C.oe_sp,
      oe_r   = test C.oe_r,
      oe_cc  = test C.oe_cc,
      clr_t  = test C.clr_t,
      inc_pc = test C.inc_pc,
      inc_sp = test C.inc_sp,
      dec_sp = test C.dec_sp,
      f3     = test C.f3,
      f2     = test C.f2,
      f1     = test C.f1,
      f0     = test C.f0,
      mr     = test C.mr,
      mw     = test C.mw,
      g14    = test C.g14,
      g13    = test C.g13,
      g12    = test C.g12,
      g11    = test C.g11,
      g10    = test C.g10,
      g9     = test C.g9,
      g8     = test C.g8,
      g7     = test C.g7,
      g6     = test C.g6,
      g5     = test C.g5,
      g4     = test C.g4,
      g3     = test C.g3,
      g2     = test C.g2,
      g1     = test C.g1,
      g0     = test C.g0
    }
