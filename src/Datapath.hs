{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Datapath where

import qualified Types.Inputs as I
import qualified Types.Byte as B
import qualified Types.Bit as Bit
import qualified Types.Bus
import qualified Types.Register as R
import qualified Types.Memory as M

data Datapath = Datapath {
  reg_a  :: R.Register,
  reg_t  :: R.Register,
  reg_x  :: R.Register,
  reg_y  :: R.Register,
  reg_pc :: R.Register,
  reg_sp :: R.Register,
  reg_ta :: R.Register,
  reg_r  :: R.Register,
  reg_cc :: R.Register,
  reg_i  :: R.Register,
  memory :: M.Memory
} deriving (Show)

defaultValue :: Datapath
defaultValue =
  let
    zero = B.zero
  in Datapath {
    reg_a = zero,
    reg_t = zero,
    reg_x = zero,
    reg_y = zero,
    reg_pc = zero,
    reg_sp = zero,
    reg_ta = zero,
    reg_r = zero,
    reg_cc = zero,
    reg_i = zero,
    memory = M.zero
  }

clock :: Datapath -> I.Inputs -> Datapath
clock
  (Datapath {
    reg_a = reg_a,
    reg_t = reg_t,
    reg_x = reg_x,
    reg_y = reg_y,
    reg_pc = reg_pc,
    reg_sp = reg_sp,
    reg_ta = reg_ta,
    reg_r = reg_r,
    reg_cc = reg_cc,
    reg_i = reg_i,
    memory = memory
  })
  (I.Inputs {
    ld_a = ld_a,
    ld_t = ld_t,
    ld_x = ld_x,
    ld_y = ld_y,
    ld_pc = ld_pc,
    ld_sp = ld_sp,
    ld_ta = ld_ta,
    ld_r = ld_r,
    ld_cc = ld_cc,
    ld_i = ld_i,
    oe_a = oe_a,
    oe_x = oe_x,
    oe_y = oe_y,
    oe_pc = oe_pc,
    oe_sp = oe_sp,
    oe_r = oe_r,
    oe_cc = oe_cc,
    clr_t = clr_t,
    inc_pc = inc_pc,
    inc_sp = inc_sp,
    dec_sp = dec_sp,
    f3 = f3,
    f2 = f2,
    f1 = f1,
    f0 = f9,
    mr = mr,
    mw = mw,
    g14 = g14,
    g13 = g13,
    g12 = g12,
    g11 = g11,
    g10 = g10,
    g9 = g9,
    g8 = g8,
    g7 = g7,
    g6 = g6,
    g5 = g5,
    g4 = g4,
    g3 = g3,
    g2 = g2,
    g1 = g1,
    g0 = g0
  }) =
  let

    address =
      if g14 == Bit.One
      then reg_ta
      else
        case (Bit.bitsToInt [ g13, g12 ]) of
          0 -> reg_pc
          1 -> reg_sp
          2 -> reg_y
          3 -> reg_x
          _ -> error "Something went wrong, should not be possible to get values other than [0..3] from a 2-bit value!"

    aluResult = B.fromInt 0x00

    newCC = B.fromInt 0x00

    -- We cheat by seeing the bus as all zeroes when nothing is open, instead of undefined
    bus =
      let
        read = R.read
        (+) = Types.Bus.add
        bus' =
          read reg_a oe_a +
          read reg_x oe_x +
          read reg_y oe_y +
          read reg_pc oe_pc +
          read reg_sp oe_sp +
          read reg_r oe_r +
          read reg_cc oe_cc +
          M.read memory address mr
      in case bus' of
        Nothing -> B.fromInt 0x00
        (Just busData) -> busData

    -- Reg T
    reg_t_data = case clr_t of
      Bit.Zero -> bus
      Bit.One  -> B.zero

    ld_t' = clr_t `Bit.or` ld_t

    -- Reg PC
    reg_pc_data = case inc_pc of
      Bit.Zero -> bus
      Bit.One  -> fst $ B.add reg_pc B.zero Bit.One -- Add one

    ld_pc' = inc_pc `Bit.or` ld_pc

    -- Reg SP
    reg_sp_data = case (inc_sp `Bit.or` dec_sp) of
      Bit.Zero -> bus
      Bit.One  -> if dec_sp == Bit.One -- If both dec and inc sp are set then dec wins
                  then fst $ B.add reg_sp B.full Bit.Zero -- Subtract one
                  else fst $ B.add reg_sp B.zero Bit.One -- Add one

    ld_sp' = inc_sp `Bit.or` dec_sp `Bit.or` ld_sp

    write = R.write
  in Datapath {
    reg_a = write reg_a bus ld_a,
    reg_t = write reg_t reg_t_data ld_t',
    reg_x = write reg_x bus ld_x,
    reg_y = write reg_y bus ld_y,
    reg_pc = write reg_pc reg_pc_data ld_pc',
    reg_sp = write reg_sp reg_sp_data ld_sp',
    reg_ta = write reg_ta bus ld_ta,
    reg_r = write reg_r aluResult ld_r,
    reg_cc = write reg_cc newCC ld_cc,
    reg_i = write reg_i bus ld_i,
    memory = M.write memory address mw bus
  }
