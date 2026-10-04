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

    im_init ld_reg = [ld_reg, mr, inc_pc]
    ab_init = [ld_ta, mr, inc_pc]
    id_init = [ld_t, mr, inc_pc]

    ab_snd ld_reg = [ld_reg, g14, mr]
    id_sp_snd ld_reg = [ld_reg, g12, mr]

    adda_calc = [oe_a, ld_r, f3, f1, f0, ld_cc]
    adca_calc = g1:adda_calc
    ad_end = [ld_a, oe_r, nf]

  in
    if counter <= 3
    then case counter of
      0 -> ins [ld_r, f1, f0, g11, ld_cc]
      1 -> ins [oe_r, ld_ta]
      2 -> ins [mr, g14, ld_pc]
      _ -> ins [mr, ld_i, inc_pc, clr_t]
    else case instruction of
      0x00 -> ins [nf]
      0x95 -> case counter of
          4 -> ins $ im_init ld_t
          5 -> ins adca_calc
          _ -> ins ad_end
      0x96 -> case counter of
          4 -> ins $ im_init ld_t
          5 -> ins adda_calc
          _ -> ins ad_end
      0xa5 -> case counter of
          4 -> ins ab_init
          5 -> ins $ ab_snd ld_t
          6 -> ins adca_calc
          _ -> ins ad_end
      0xb5 -> case counter of
          4 -> ins id_init
          5 -> ins $ id_sp_snd ld_t
          6 -> ins adca_calc
          _ -> ins ad_end
      0xe1 -> case counter of
          4 -> ins ab_init
          _ -> ins [g14, mw, oe_a, nf]
      0xf0 -> ins [ld_a, mr, inc_pc, ld_cc, f3, f0, g5, g3, g2, nf]
      _    -> ins []
