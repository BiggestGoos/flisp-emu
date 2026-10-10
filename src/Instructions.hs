module Instructions where

import qualified Types.Byte as Byte
import qualified Types.Bit as Bit
import qualified Types.Inputs as Inputs
import qualified Counter

import Types.Inputs.Constants

import Numeric (showHex)

type ControlSignals = (Bit.Bit, Inputs.Inputs)

create :: Counter.Counter -> Byte.Byte -> Byte.Byte -> ControlSignals
create (Counter.Counter q3 q2 q1 q0) reg_i (Byte.Byte _ _ _ i n z v c) =
  let
    instruction = Byte.toInt reg_i
    counter = Bit.bitsToInt [q3, q2, q1, q0]

    nf = 42
    ins inputs =
      (
        if nf `elem` inputs
        then Bit.One
        else Bit.Zero,
        Inputs.create inputs
      )

    nop = [nf]

    im_init ld_reg = [ld_reg, mr, inc_pc]
    ab_init = [ld_ta, mr, inc_pc]

    id_init = [ld_t, mr, inc_pc]
    aid_init = [oe_a, ld_t]

    ab_snd load = [load, g14, mr]

    id_sp_snd load = [load, g12, mr]
    id_x_snd load = [load, g13, g12, mr]

    add_calc open = [open, ld_r, f3, f1, f0]

    adda_calc = ld_cc:(add_calc oe_a)
    adca_calc = g1:adda_calc

    inc_calc open = [open, f3, f0, g0, ld_r, ld_cc, g3, g2]
    dec_calc open = [open, f3, f1, ld_r, ld_cc, g3, g2]

    lsr_calc open = [open, f3, f2, f1, ld_r, ld_cc, g9]

    rol_calc open = [open, f3, f2, f0, ld_r, ld_cc, g1]

    calc_end load = [load, oe_r, nf]

    cmp_calc open = [open, f3, f2, ld_cc, g0]
    tst_calc open = [open, f3, f0, ld_cc, g5, g4, g3, g2]

    b_init = im_init ld_t
    b_calc = [oe_pc, f3, f1, f0, ld_r]
    b_store = [oe_r, ld_pc, nf]

    branch_if state cond = case state of
      4 -> b_init
      5 -> b_calc
      _ -> if cond
           then b_store
           else nop

    bitwise_and open load = [open, f2, f1, f0] ++ (if load then [ld_r] else [])
    bitwise_and' open load = (bitwise_and open load) ++ cc_nz0_
    bitwise_or open = [open, ld_r, f2, f1]
    bitwise_or' open = (bitwise_or open) ++ cc_nz0_

    cc_nz0_ = [ld_cc, g5, g3, g2]
    cc_rebind = [ld_cc, g8, g6, g4, g2]

    ld_base = [f3, f0, nf] ++ cc_nz0_
    ld_reg_im load = [load, mr, inc_pc] ++ ld_base
    ld_reg_ab load = [load, mr, g14] ++ ld_base
    ld_reg_ns load = [load, mr, g12] ++ ld_base
    ld_reg_nx load = [load, mr, g13, g12] ++ ld_base
    ld_reg_ax = ld_reg_nx

    st_base = [mw, nf]
    st_reg_ab open = [open, g14] ++ st_base
    st_reg_ns open = [open, g12] ++ st_base

    psh_reg state open = case state of
      4 -> [dec_sp]
      _ -> [open, mw, g12, nf]
    pul_reg state load = case state of
      4 -> [load, mr, g12]
      _ -> [inc_sp, nf]

    exg_regs state open1 load1 open2 load2 = case state of
      4 -> [open1, f3, f0, ld_r]
      5 -> [load1, open2]
      _ -> cc_rebind ++ (calc_end load2)

  in ins $
    if counter <= 3
    then case counter of
      0 -> [ld_r, f1, f0, g11, ld_cc]
      1 -> [oe_r, ld_ta]
      2 -> [mr, g14, ld_pc]
      _ -> [mr, ld_i, inc_pc, clr_t]
    else case instruction of
      0x00 -> nop
      0x01 -> case counter of
          4 -> im_init ld_t
          5 -> bitwise_and oe_cc True
          _ -> cc_rebind ++ (calc_end ld_cc)
      0x05 -> case counter of
          4 -> [ld_r, ld_cc]
          _ -> calc_end ld_a
      0x07 -> case counter of
          4 -> inc_calc oe_a
          _ -> calc_end ld_a
      0x08 -> case counter of
          4 -> dec_calc oe_a
          _ -> calc_end ld_a
      0x10 -> psh_reg counter oe_a
      0x11 -> psh_reg counter oe_x
      0x13 -> psh_reg counter oe_cc
      0x14 -> pul_reg counter ld_a
      0x15 -> pul_reg counter ld_x
      0x20 -> case counter of
          4 -> dec_sp:b_init
          5 -> clr_t:b_calc
          6 -> [oe_pc, mw, g12]
          _ -> b_store
      0x21 -> branch_if counter True
      0x24 -> branch_if counter (z == Bit.One)
      0x25 -> branch_if counter (z == Bit.Zero)
      0x2c -> branch_if counter (((n `Bit.xor` v) `Bit.or` z) == Bit.Zero)
      0x33 -> [ld_pc, mr, nf]
      0x37 -> case counter of
          4 -> ab_init
          5 -> g14:(inc_calc mr)
          _ -> g14:(calc_end mw)
      0x38 -> case counter of
          4 -> ab_init
          5 -> g14:(dec_calc mr)
          _ -> g14:(calc_end mw)
      0x43 -> [inc_sp, mr, g12, ld_pc, nf]
      0x48 -> case counter of
          4 -> im_init ld_t
          5 -> g12:(dec_calc mr)
          _ -> g12:(calc_end mw)
      0x4c -> case counter of
          4 -> im_init ld_t
          5 -> g12:(lsr_calc mr)
          _ -> g12:(calc_end mw)
      0x4d -> case counter of
          4 -> im_init ld_t
          5 -> g12:(rol_calc mr)
          _ -> g12:(calc_end mw)
      0x69 -> case counter of
          4 -> aid_init
          _ -> g13:g12:(tst_calc mr) ++ [nf]
      0x90 -> ld_reg_im ld_x
      0x92 -> ld_reg_im ld_sp
      0x95 -> case counter of
          4 -> im_init ld_t
          5 -> adca_calc
          _ -> calc_end ld_a
      0x96 -> case counter of
          4 -> im_init ld_t
          5 -> adda_calc
          _ -> calc_end ld_a
      0x97 -> case counter of
          4 -> im_init ld_t
          _ -> nf:(cmp_calc oe_a)
      0x98 -> case counter of
          4 -> im_init ld_t
          _ -> (bitwise_and' oe_a False) ++ [nf]
      0x99 -> case counter of
          4 -> im_init ld_t
          5 -> (bitwise_and' oe_a True)
          _ -> calc_end ld_a
      0x9f -> exg_regs counter oe_a ld_a oe_cc ld_cc
      0xa0 -> case counter of
          4 -> ab_init
          _ -> ld_reg_ab ld_x
      0xa5 -> case counter of
          4 -> ab_init
          5 -> ab_snd ld_t
          6 -> adca_calc
          _ -> calc_end ld_a
      0xb2 -> case counter of
          4 -> im_init ld_t
          _ -> ld_reg_ns ld_sp
      0xb5 -> case counter of
          4 -> id_init
          5 -> id_sp_snd ld_t
          6 -> adca_calc
          _ -> calc_end ld_a
      0xba -> case counter of
          4 -> im_init ld_t
          5 -> id_sp_snd ld_t
          6 -> bitwise_or' oe_a
          _ -> calc_end ld_a
      0xbe -> case counter of
          4 -> im_init ld_t
          5 -> add_calc oe_sp
          _ -> calc_end ld_sp
      0xc2 -> case counter of
          4 -> im_init ld_t
          _ -> ld_reg_nx ld_sp
      0xc6 -> case counter of
          4 -> id_init
          5 -> id_x_snd ld_t
          6 -> adda_calc
          _ -> calc_end ld_a
      0xcc -> case counter of
          4 -> im_init ld_t
          5 -> add_calc oe_x
          _ -> calc_end ld_x
      0xdc -> case counter of
          4 -> im_init ld_t
          5 -> add_calc oe_sp
          _ -> calc_end ld_x
      0xe1 -> case counter of
          4 -> ab_init
          _ -> st_reg_ab oe_a
      0xe2 -> case counter of
          4 -> id_init
          _ -> st_reg_ns oe_a
      0xf0 -> ld_reg_im ld_a
      0xf1 -> case counter of
          4 -> ab_init
          _ -> ld_reg_ab ld_a
      0xf2 -> case counter of
          4 -> id_init
          _ -> ld_reg_ns ld_a
      0xf3 -> case counter of
          4 -> id_init
          _ -> ld_reg_nx ld_a
      0xf4 -> case counter of
          4 -> aid_init
          _ -> ld_reg_ax ld_a

      -- Temporary, disregard
      --
      -- MOVE #Data,Adr
      0xdf -> case counter of
          4 -> [mr, ld_ta, inc_pc]
          5 -> [mr, ld_r, f3, f0, inc_pc]
          _ -> [oe_r, mw, g14, nf]
      0xef -> case counter of
          4 -> [mr, ld_r, f3, f0, inc_pc]
          5 -> [mr, ld_t, inc_pc]
          6 -> cmp_calc oe_a
          _ -> if z == Bit.One
               then [oe_r, ld_pc, nf]
               else nop

      _    -> error $ "Instruction " ++ (showHex instruction "") ++ " is not implemented!" -- This should throw an interrupt, this is temporary
