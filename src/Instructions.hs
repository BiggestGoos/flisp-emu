module Instructions where

import qualified Types.Byte as Byte
import qualified Types.Bit as Bit
import qualified Types.Inputs as Inputs
import qualified Counter

create :: Counter.Counter -> Byte.Byte -> Byte.Byte -> (Bit.Bit, Inputs.Inputs)
create (Counter.Counter q3 q2 q1 q0) reg_i (Byte.Byte _ _ _ i n z v c) =
  let
    instruction = Byte.toInt reg_i
    counter = Bit.bitsToInt [q3, q2, q1, q0]

    ins inputs =
      (
        if "nf" `elem` inputs
        then Bit.One
        else Bit.Zero,
        Inputs.create inputs
      )
  in
    if counter <= 3
    then case counter of
      0 -> ins ["ld_r", "f1", "f0", "g11", "ld_cc"]
      1 -> ins ["oe_r", "ld_ta"]
      2 -> ins ["mr", "g14", "ld_pc"]
      3 -> ins ["mr", "ld_i", "inc_pc", "clr_t"]
      _ -> error "Wrong!"
    else case instruction of
      0x00 -> ins ["nf"]
      0xf0 -> ins ["ld_a", "mr", "inc_pc", "ld_cc", "f3", "f0", "g5", "g3", "g2", "nf"]
      _    -> ins []
