--{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Datapath where

import qualified Types.Inputs as I
import qualified Types.Byte as B
import qualified Types.Bit as Bit
import qualified Types.Bus
import qualified Types.Register as R
import qualified Types.Memory as M
import qualified ALU

data Datapath = Datapath {
  reg_a,
  reg_t,
  reg_x,
  reg_y,
  reg_pc,
  reg_sp,
  reg_ta,
  reg_r,
  reg_cc,
  reg_i  :: R.Register,
  memory :: M.Memory
} deriving (Eq, Show)

flash :: M.Memory -> Datapath
flash memory =
  let
    zero = B.zero
  in Datapath {
    reg_a  = zero,
    reg_t  = zero,
    reg_x  = zero,
    reg_y  = zero,
    reg_pc = zero,
    reg_sp = zero,
    reg_ta = zero,
    reg_r  = zero,
    reg_cc = zero,
    reg_i  = zero,
    memory = memory
  }

clock :: Datapath -> I.Inputs -> Datapath
clock
  (Datapath {
    reg_a,
    reg_t,
    reg_x,
    reg_y,
    reg_pc,
    reg_sp,
    reg_ta,
    reg_r,
    reg_cc,
    reg_i,
    memory
  })
  (I.Inputs {
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
    oe_a,
    oe_x,
    oe_y,
    oe_pc,
    oe_sp,
    oe_r,
    oe_cc,
    clr_t,
    inc_pc,
    inc_sp,
    dec_sp,
    f3,
    f2,
    f1,
    f0,
    mr,
    mw,
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
    g0
  }) =
  let

    pickSource s1 s0 v0 v1 v2 v3 = case (Bit.bitsToInt [ s1, s0 ]) of
      0 -> v0
      1 -> v1
      2 -> v2
      3 -> v3
      _ -> error "Something went wrong, should not be possible to get values other than [0..3] from a 2-bit value!"

    address =
      if g14 == Bit.One
      then reg_ta
      else pickSource g13 g12 reg_pc reg_sp reg_y reg_x

    -- We cheat by seeing the bus as all zeroes when nothing is open, instead of undefined
    bus@(B.Byte _ _ _ bus_i bus_n bus_z bus_v bus_c) =
      let
        regRead = R.read
        (<->) = Types.Bus.add
        bus' =
          regRead reg_a  oe_a  <->
          regRead reg_x  oe_x  <->
          regRead reg_y  oe_y  <->
          regRead reg_pc oe_pc <->
          regRead reg_sp oe_sp <->
          regRead reg_r  oe_r  <->
          regRead reg_cc oe_cc <->
          M.read  memory address mr
      in case bus' of
        Nothing        -> B.fromInt 0x00
        (Just busData) -> busData

    -- ALU and CC
    (B.Byte _ _ _ old_i old_n old_z old_v old_c) = reg_cc

    (aluResult, (ALU.Flags alu_n alu_z alu_v alu_c)) =
      let
        function = ALU.Function f3 f2 f1 f0
        cin = pickSource g1 g0 Bit.Zero Bit.One old_c (Bit.not old_c)
      in
        ALU.alu function bus reg_t cin

    newCC =
      let
        new_i = pickSource g11 g10 old_i bus_i Bit.One  old_i
        new_n = pickSource g9  g8  alu_n bus_n Bit.Zero old_n
        new_z = pickSource g7  g6  alu_z bus_z Bit.Zero old_z
        new_v = pickSource g5  g4  alu_v bus_v Bit.Zero old_v
        new_c = pickSource g3  g2  alu_c bus_c Bit.Zero old_c
      in
        B.Byte Bit.Zero Bit.Zero Bit.Zero new_i new_n new_z new_v new_c

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
                  else fst $ B.add reg_sp B.zero Bit.One  -- Add one

    ld_sp' = inc_sp `Bit.or` dec_sp `Bit.or` ld_sp

    regWrite = R.write
  in Datapath {
    reg_a  = regWrite reg_a  bus         ld_a,
    reg_t  = regWrite reg_t  reg_t_data  ld_t',
    reg_x  = regWrite reg_x  bus         ld_x,
    reg_y  = regWrite reg_y  bus         ld_y,
    reg_pc = regWrite reg_pc reg_pc_data ld_pc',
    reg_sp = regWrite reg_sp reg_sp_data ld_sp',
    reg_ta = regWrite reg_ta bus         ld_ta,
    reg_r  = regWrite reg_r  aluResult   ld_r,
    reg_cc = regWrite reg_cc newCC       ld_cc,
    reg_i  = regWrite reg_i  bus         ld_i,
    memory = M.write  memory address     mw bus
  }
