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

create :: [String] -> Inputs
create inputs =
  let
    test name = if name `elem` inputs then One else Zero
  in
    Inputs {
      ld_a   = test "ld_a",
      ld_t   = test "ld_t",
      ld_x   = test "ld_x",
      ld_y   = test "ld_y",
      ld_pc  = test "ld_pc",
      ld_sp  = test "ld_sp",
      ld_ta  = test "ld_ta",
      ld_r   = test "ld_r",
      ld_cc  = test "ld_cc",
      ld_i   = test "ld_i",
      oe_a   = test "oe_a",
      oe_x   = test "oe_x",
      oe_y   = test "oe_y",
      oe_pc  = test "oe_pc",
      oe_sp  = test "oe_sp",
      oe_r   = test "oe_r",
      oe_cc  = test "oe_cc",
      clr_t  = test "clr_t",
      inc_pc = test "inc_pc",
      inc_sp = test "inc_sp",
      dec_sp = test "dec_sp",
      f3     = test "f3",
      f2     = test "f2",
      f1     = test "f1",
      f0     = test "f0",
      mr     = test "mr",
      mw     = test "mw",
      g14    = test "g14",
      g13    = test "g13",
      g12    = test "g12",
      g11    = test "g11",
      g10    = test "g10",
      g9     = test "g9",
      g8     = test "g8",
      g7     = test "g7",
      g6     = test "g6",
      g5     = test "g5",
      g4     = test "g4",
      g3     = test "g3",
      g2     = test "g2",
      g1     = test "g1",
      g0     = test "g0"
    }
