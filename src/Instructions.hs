module Instructions where

import qualified Types.Byte as Byte
import qualified Types.Bit as Bit
import qualified Types.Inputs as Inputs
import qualified Counter

import Types.Inputs.Constants

create :: Counter.Counter -> Byte.Byte -> Byte.Byte -> (Bit.Bit, Inputs.Inputs)
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

    ab_snd load = [load, g14, mr]
    id_sp_snd load = [load, g12, mr]

    adda_calc = [oe_a, ld_r, f3, f1, f0, ld_cc]
    adca_calc = g1:adda_calc

    inc_calc open = [open, f3, f0, g0, ld_r, ld_cc, g3, g2]
    dec_calc open = [open, f3, f1, ld_r, ld_cc, g3, g2]

    calc_end load = [load, oe_r, nf]

    cmp_calc open = [open, f3, f2, g0, ld_cc]

    b_init = im_init ld_t
    b_calc = [oe_pc, f3, f1, f0, ld_r]
    b_store = [oe_r, ld_pc, nf]

  in
    if counter <= 3
    then case counter of
      0 -> ins [ld_r, f1, f0, g11, ld_cc]
      1 -> ins [oe_r, ld_ta]
      2 -> ins [mr, g14, ld_pc]
      _ -> ins [mr, ld_i, inc_pc, clr_t]
    else case instruction of
      0x00 -> ins nop
      0x05 -> case counter of
          4 -> ins [ld_r, ld_cc]
          _ -> ins $ calc_end ld_a
      0x07 -> case counter of
          4 -> ins $ inc_calc oe_a
          _ -> ins $ calc_end ld_a
      0x08 -> case counter of
          4 -> ins $ dec_calc oe_a
          _ -> ins $ calc_end ld_a
      0x21 -> case counter of
          4 -> ins b_init
          5 -> ins b_calc
          _ -> ins b_store
      0x24 -> case counter of
          4 -> ins b_init
          5 -> ins b_calc
          _ -> if z == Bit.One
               then ins b_store
               else ins nop
      0x33 -> ins [ld_pc, mr, nf]
      0x38 -> case counter of
          4 -> ins $ ab_init
          5 -> ins $ g14:(dec_calc mr)
          _ -> ins $ g14:(calc_end mw)
      0x95 -> case counter of
          4 -> ins $ im_init ld_t
          5 -> ins adca_calc
          _ -> ins $ calc_end ld_a
      0x96 -> case counter of
          4 -> ins $ im_init ld_t
          5 -> ins adda_calc
          _ -> ins $ calc_end ld_a
      0xa5 -> case counter of
          4 -> ins ab_init
          5 -> ins $ ab_snd ld_t
          6 -> ins adca_calc
          _ -> ins $ calc_end ld_a
      0xb5 -> case counter of
          4 -> ins id_init
          5 -> ins $ id_sp_snd ld_t
          6 -> ins adca_calc
          _ -> ins $ calc_end ld_a
      0xe1 -> case counter of
          4 -> ins ab_init
          _ -> ins [g14, mw, oe_a, nf]
      0xf0 -> ins [ld_a, mr, inc_pc, ld_cc, f3, f0, g5, g3, g2, nf]

      -- Temporary, disregard
      --
      -- MOVE #Data,Adr
      0xdf -> case counter of
          4 -> ins [mr, ld_ta, inc_pc]
          5 -> ins [mr, ld_r, f3, f0, inc_pc]
          _ -> ins [oe_r, mw, g14, nf]
      0xef -> case counter of
          4 -> ins [mr, ld_r, f3, f0, inc_pc]
          5 -> ins [mr, ld_t, inc_pc]
          6 -> ins $ cmp_calc oe_a
          _ -> if z == Bit.One
               then ins [oe_r, ld_pc, nf]
               else ins nop

      _    -> ins []
