module Instructions where

import qualified Types.Byte as Byte
import qualified Types.Bit as Bit
import qualified Types.Inputs as Inputs
import qualified Counter

import Types.Inputs.Constants

type ControlSignals = (Bit.Bit, Inputs.Inputs, Maybe Bit.Bit)

data CurrentState = HW_Int | Reset | Normal deriving (Eq)

create :: Counter.Counter -> Byte.Byte -> Byte.Byte -> (Maybe Bit.Bit) -> ControlSignals
create (Counter.Counter q3 q2 q1 q0) reg_i (Byte.Byte _ _ _ i n z v c) irq =
  let
    instruction = Byte.toInt reg_i
    counter = Bit.bitsToInt [q3, q2, q1, q0]

    nf = 42
    irq_start = 43
    irq_ack = 44
    ins inputs =
      (
        if nf `elem` inputs
        then Bit.One
        else Bit.Zero,
        Inputs.create inputs,
        if irq_start `elem` inputs
        then Nothing
        else
          if irq_ack `elem` inputs
          then Just Bit.Zero
          else irq
      )

    nop = [nf]

    im_init ld_reg = [ld_reg, mr, inc_pc]
    ab_init = [ld_ta, mr, inc_pc]

    id_init = [ld_t, mr, inc_pc]
    aid_init = [oe_a, ld_t]

    ab_mr = [g14, mr]
    id_sp_mr = [g12, mr]
    id_x_mr = [g13, g12, mr]
    id_y_mr  = [g13, mr]

    ab_mw = [g14, mw]
    id_sp_mw = [g12, mw]
    id_x_mw = [g13, g12, mw]
    id_y_mw  = [g13, mw]

    add_calc = [ld_r, f3, f1, f0]

    adda_calc = ld_cc:(oe_a:add_calc)
    adca_calc = g1:adda_calc

    anda_calc = bitwise_and' oe_a True
    ora_calc = bitwise_or' oe_a

    asl_calc = [f3, f2, f0, ld_r, ld_cc]
    asr_calc = [f3, f2, f1, f0, ld_r, ld_cc, g5]

    clr_calc = [ld_r, ld_cc]

    com_calc' = [f2, f0, ld_r]
    com_calc = com_calc' ++ cc_nz0_

    eor_calc = [f3, ld_r] ++ cc_nz0_

    neg_calc = ld_cc:g0:com_calc'

    sub_calc = [ld_r, f3, f2]

    suba_calc = ld_cc:g0:(oe_a:sub_calc)
    sbca_calc = ld_cc:g1:g0:(oe_a:sub_calc)

    inc_calc' = [f3, f0, g0, ld_r]
    inc_calc = inc_calc' ++ [ld_cc, g3, g2]
    dec_calc' = [f3, f1, ld_r]
    dec_calc = dec_calc' ++ [ld_cc, g3, g2]

    lsr_calc = [f3, f2, f1, ld_r, ld_cc, g9]

    ror_calc = [ f3, f2, f1, ld_r, ld_cc, g1]
    rol_calc = [f3, f2, f0, ld_r, ld_cc, g1]

    calc_end' = [oe_r]
    calc_end = nf:calc_end'
    calc_end_a = ld_a:calc_end

    bita_calc_end = nf:(bitwise_and' oe_a False)
    cmp_calc_end = [f3, f2, ld_cc, g0, nf]
    tst_calc_end = [f3, f0, ld_cc, g5, g4, g3, g2, nf]

    b_init = im_init ld_t
    b_calc = [oe_pc, f3, f1, f0, ld_r]
    b_store = [oe_r, ld_pc, nf]

    branch_if cond = [
        b_init,
        b_calc,
        if cond
          then b_store
          else nop
      ]

    bitwise_and open load = [open, f2, f1, f0] ++ (if load then [ld_r] else [])
    bitwise_and' open load = (bitwise_and open load) ++ cc_nz0_
    bitwise_or open = [open, ld_r, f2, f1]
    bitwise_or' open = (bitwise_or open) ++ cc_nz0_

    cc_nz0_ = [ld_cc, g5, g3, g2]
    cc_rebind = [ld_cc, g8, g6, g4, g2]
    cc_rebind' = cc_rebind ++ [g10]

    ld_base = [mr, f3, f0] ++ cc_nz0_
    ld_reg_im load = [load, inc_pc, nf] ++ ld_base
    ld_reg_ab load = [load, g14, nf] ++ ld_base
    ld_reg_ns load = [load, g12, nf] ++ ld_base
    ld_reg_nx' load = [load, g13, g12] ++ ld_base
    ld_reg_nx load = nf:(ld_reg_nx' load)
    ld_reg_ax = ld_reg_nx
    ld_reg_ny' load = [load, g13] ++ ld_base
    ld_reg_ny load = nf:(ld_reg_ny' load)
    ld_reg_ay = ld_reg_ny

    st_base = [mw]
    st_reg_ab open = [open, g14, nf] ++ st_base
    st_reg_ns open = [open, g12, nf] ++ st_base
    st_reg_nx' open = [open, g12, g13] ++ st_base
    st_reg_nx open = nf:(st_reg_nx open)
    st_reg_ax = st_reg_nx
    st_reg_ny' open = [open, g13] ++ st_base
    st_reg_ny open = nf:(st_reg_ny open)
    st_reg_ay = st_reg_ny

    psh_reg open = [
        [dec_sp],
        [open, mw, g12, nf]
      ]
    pul_reg load = [
        [mr, g12] ++ if load == ld_cc then cc_rebind else [load],
        [inc_sp, nf]
      ]

    exg_regs' cc open1 load1 open2 load2 = [
        [open1, f3, f0, ld_r],
        [load1, open2],
        load2:calc_end ++ if cc then cc_rebind else []
      ]
    exg_regs = exg_regs' False

    tfr_regs open load = [
        [open, load] ++ if load == ld_cc then cc_rebind else []
      ]

    hw_interrupt = case counter of
      3  -> [irq_start, dec_sp, clr_t]
      4  -> [oe_pc, dec_sp, mw, g12]
      5  -> [oe_y, dec_sp, mw, g12]
      6  -> [oe_x, dec_sp, mw, g12]
      7  -> [oe_a, dec_sp, mw, g12]
      8  -> [oe_cc, mw, g12, g11, g9, g8, g7, g6, g5, g4, g3, g2, ld_cc]
      9  -> [ld_r, f0]
      10 -> [oe_r, ld_ta]
      _  -> [mr, g14, ld_pc, nf, irq_ack]

    sw_interrupt = [
        [dec_sp],
        [oe_pc, dec_sp, mw, g12],
        [oe_y, dec_sp, mw, g12],
        [oe_x, dec_sp, mw, g12],
        [oe_a, dec_sp, mw, g12],
        [oe_cc, mw, g12, g11, g9, g8, g7, g6, g5, g4, g3, g2, ld_cc],
        [ld_r, f1],
        [oe_r, ld_ta],
        [mr, g14, ld_pc, nf]
      ]

    fetch_phase = 3
    exec_phase = fetch_phase + 1

    eval_state :: CurrentState
    eval_state =
      if (irq == Just Bit.One && counter == fetch_phase && i == Bit.Zero) || irq == Nothing
      then HW_Int
      else
        if counter <= 3
        then Reset
        else Normal

    ins_index = counter - exec_phase

  in ins $ case eval_state of
    HW_Int -> hw_interrupt
    Reset -> case counter of
      0 -> [ld_r, f1, f0, g11, ld_cc]
      1 -> [oe_r, ld_ta]
      2 -> [mr, g14, ld_pc]
      _ -> [mr, ld_i, inc_pc, clr_t]
    _ -> flip (!!) ins_index $ case instruction of
      -- ADCA
      0x95 -> [
          im_init ld_t,
          adca_calc,
          calc_end_a
        ]
      0xa5 -> [
          ab_init,
          ld_t:ab_mr,
          adca_calc,
          calc_end_a
        ]
      0xb5 -> [
          id_init,
          ld_t:id_sp_mr,
          adca_calc,
          calc_end_a
        ]
      0xc5 -> [
          id_init,
          ld_t:id_x_mr,
          adca_calc,
          calc_end_a
        ]
      0xd5 -> [
          id_init,
          ld_t:id_y_mr,
          adca_calc,
          calc_end_a
        ]
      -- ADDA
      0x96 -> [
          im_init ld_t,
          adda_calc,
          calc_end_a
        ]
      0xa6 -> [
          ab_init,
          ld_t:ab_mr,
          adda_calc,
          calc_end_a
        ]
      0xb6 -> [
          id_init,
          ld_t:id_sp_mr,
          adda_calc,
          calc_end_a
        ]
      0xc6 -> [
          id_init,
          ld_t:id_x_mr,
          adda_calc,
          calc_end_a
        ]
      0xd6 -> [
          id_init,
          ld_t:id_y_mr,
          adda_calc,
          calc_end_a
        ]
      -- ANDA
      0x99 -> [
          im_init ld_t,
          anda_calc,
          calc_end_a
        ]
      0xa9 -> [
          ab_init,
          ld_t:ab_mr,
          anda_calc,
          calc_end_a
        ]
      0xb9 -> [
          id_init,
          ld_t:id_sp_mr,
          anda_calc,
          calc_end_a
        ]
      0xc9 -> [
          id_init,
          ld_t:id_x_mr,
          anda_calc,
          calc_end_a
        ]
      0xd9 -> [
          id_init,
          ld_t:id_y_mr,
          anda_calc,
          calc_end_a
        ]
      -- ANDCC
      0x01 -> [
          im_init ld_t,
          bitwise_and oe_cc True,
          cc_rebind' ++ calc_end
        ]
      -- ASL
      0x0b -> [
          oe_a:asl_calc,
          calc_end_a
        ]
      0x3b -> [
          ab_init,
          ab_mr ++ asl_calc,
          ab_mw ++ calc_end
        ]
      0x4b -> [
          id_init,
          id_sp_mr ++ asl_calc,
          id_sp_mw ++ calc_end
        ]
      0x5b -> [
          id_init,
          id_x_mr ++ asl_calc,
          id_x_mw ++ calc_end
        ]
      0x6b -> [
          aid_init,
          id_x_mr ++ asl_calc,
          id_x_mw ++ calc_end
        ]
      0x7b -> [
          id_init,
          id_y_mr ++ asl_calc,
          id_y_mw ++ calc_end
        ]
      0x8b -> [
          aid_init,
          id_y_mr ++ asl_calc,
          id_y_mw ++ calc_end
        ]
      -- ASR
      0x0f -> [
          oe_a:asr_calc,
          calc_end_a
        ]
      0x3f -> [
          ab_init,
          ab_mr ++ asr_calc,
          ab_mw ++ calc_end
        ]
      0x4f -> [
          id_init,
          id_sp_mr ++ asr_calc,
          id_sp_mw ++ calc_end
        ]
      0x5f -> [
          id_init,
          id_x_mr ++ asr_calc,
          id_x_mw ++ calc_end
        ]
      0x6f -> [
          aid_init,
          id_x_mr ++ asr_calc,
          id_x_mw ++ calc_end
        ]
      0x7f -> [
          id_init,
          id_y_mr ++ asr_calc,
          id_y_mw ++ calc_end
        ]
      0x8f -> [
          aid_init,
          id_y_mr ++ asr_calc,
          id_y_mw ++ calc_end
        ]
      -- BCC
      0x29 -> branch_if $ c == Bit.Zero
      -- BCS
      0x28 -> branch_if $ c == Bit.One
      -- BEQ
      0x24 -> branch_if $ z == Bit.One
      -- BGE
      0x2d -> branch_if $ n `Bit.xor` v == Bit.Zero
      -- BGT
      0x2c -> branch_if $ (n `Bit.xor` v) `Bit.or` z == Bit.Zero
      -- BHS (=BCC)
      -- BHI
      0x2a -> branch_if $ c `Bit.or` z == Bit.Zero
      -- BLO (=BCS)
      -- BITA
      0x98 -> [
          im_init ld_t,
          bita_calc_end
        ]
      0xa8 -> [
          ab_init,
          ld_t:ab_mr,
          bita_calc_end
        ]
      0xb8 -> [
          id_init,
          ld_t:id_sp_mr,
          bita_calc_end
        ]
      0xc8 -> [
          id_init,
          ld_t:id_x_mr,
          bita_calc_end
        ]
      0xd8 -> [
          id_init,
          ld_t:id_y_mr,
          bita_calc_end
        ]
      -- BLE
      0x2e -> branch_if $ (n `Bit.xor` v) `Bit.or` z == Bit.One
      -- BLS
      0x2b -> branch_if $ c `Bit.or` z == Bit.One
      -- BLT
      0x2f -> branch_if $ n `Bit.xor` v == Bit.One
      -- BMI
      0x22 -> branch_if $ n == Bit.One
      -- BNE
      0x25 -> branch_if $ z == Bit.Zero
      -- BPL
      0x23 -> branch_if $ n == Bit.Zero
      -- BRA
      0x21 -> branch_if $ True
      -- BSR
      0x20 -> [
          dec_sp:b_init,
          clr_t:b_calc,
          [oe_pc, mw, g12],
          b_store
        ]
      -- BVC
      0x27 -> branch_if $ v == Bit.Zero
      -- BVS
      0x26 -> branch_if $ v == Bit.One
      -- CLR
      0x05 -> [
          clr_calc,
          calc_end_a
        ]
      0x35 -> [
          ab_init ++ clr_calc,
          ab_mw ++ calc_end
        ]
      0x45 -> [
          id_init ++ clr_calc,
          id_sp_mw ++ calc_end
        ]
      0x55 -> [
          id_init ++ clr_calc,
          id_x_mw ++ calc_end
        ]
      0x65 -> [
          aid_init ++ clr_calc,
          id_x_mw ++ calc_end
        ]
      0x75 -> [
          id_init ++ clr_calc,
          id_y_mw ++ calc_end
        ]
      0x85 -> [
          aid_init ++ clr_calc,
          id_y_mw ++ calc_end
        ]
      -- CMP
      0x97 -> [
          im_init ld_t,
          oe_a:cmp_calc_end
        ]
      0xa7 -> [
          ab_init,
          ld_t:ab_mr,
          oe_a:cmp_calc_end
        ]
      0xb7 -> [
          id_init,
          ld_t:id_sp_mr,
          oe_a:cmp_calc_end
        ]
      0xc7 -> [
          id_init,
          ld_t:id_x_mr,
          oe_a:cmp_calc_end
        ]
      0xd7 -> [
          id_init,
          ld_t:id_y_mr,
          oe_a:cmp_calc_end
        ]
      0x9c -> [
          im_init ld_t,
          oe_x:cmp_calc_end
        ]
      0xac -> [
          ab_init,
          ld_t:ab_mr,
          oe_x:cmp_calc_end
        ]
      0xbc -> [
          id_init,
          ld_t:id_sp_mr,
          oe_x:cmp_calc_end
        ]
      0x9d -> [
          im_init ld_t,
          oe_y:cmp_calc_end
        ]
      0xad -> [
          ab_init,
          ld_t:ab_mr,
          oe_y:cmp_calc_end
        ]
      0xbd -> [
          id_init,
          ld_t:id_sp_mr,
          oe_y:cmp_calc_end
        ]
      0x9e -> [
          im_init ld_t,
          oe_sp:cmp_calc_end
        ]
      0xae -> [
          ab_init,
          ld_t:ab_mr,
          oe_sp:cmp_calc_end
        ]
      -- COM
      0x0a -> [
          oe_a:com_calc,
          calc_end_a
        ]
      0x3a -> [
          ab_init,
          ab_mr ++ com_calc,
          ab_mw ++ calc_end
        ]
      0x4a -> [
          id_init,
          id_sp_mr ++ com_calc,
          id_sp_mw ++ calc_end
        ]
      0x5a -> [
          id_init,
          id_x_mr ++ com_calc,
          id_x_mw ++ calc_end
        ]
      0x6a -> [
          aid_init,
          id_x_mr ++ com_calc,
          id_x_mw ++ calc_end
        ]
      0x7a -> [
          id_init,
          id_y_mr ++ com_calc,
          id_y_mw ++ calc_end
        ]
      0x8a -> [
          aid_init,
          id_y_mr ++ com_calc,
          id_y_mw ++ calc_end
        ]
      -- DEC
      0x08 -> [
          oe_a:dec_calc,
          calc_end_a
        ]
      0x38 -> [
          ab_init,
          ab_mr ++ dec_calc,
          ab_mw ++ calc_end
        ]
      0x48 -> [
          id_init,
          id_sp_mr ++ dec_calc,
          id_sp_mw ++ calc_end
        ]
      0x58 -> [
          id_init,
          id_x_mr ++ dec_calc,
          id_x_mw ++ calc_end
        ]
      0x68 -> [
          aid_init,
          id_x_mr ++ dec_calc,
          id_x_mw ++ calc_end
        ]
      0x78 -> [
          id_init,
          id_y_mr ++ dec_calc,
          id_y_mw ++ calc_end
        ]
      0x88 -> [
          aid_init,
          id_y_mr ++ dec_calc,
          id_y_mw ++ calc_end
        ]
      -- EOR
      0x9b -> [
          oe_a:eor_calc,
          calc_end_a
        ]
      0xab -> [
          ab_init,
          ab_mr ++ eor_calc,
          ab_mw ++ calc_end
        ]
      0xbb -> [
          id_init,
          id_sp_mr ++ eor_calc,
          id_sp_mw ++ calc_end
        ]
      0xcb -> [
          id_init,
          id_x_mr ++ eor_calc,
          id_x_mw ++ calc_end
        ]
      0xdb -> [
          id_init,
          id_y_mr ++ eor_calc,
          id_y_mw ++ calc_end
        ]
      -- EXG
      0x9f -> exg_regs' True oe_a ld_a oe_cc ld_cc
      0xaf -> exg_regs oe_x ld_x oe_y ld_y
      0xbf -> exg_regs oe_x ld_x oe_sp ld_sp
      0xcf -> exg_regs oe_y ld_y oe_sp ld_sp
      -- INC
      0x07 -> [
          oe_a:inc_calc,
          calc_end_a
        ]
      0x37 -> [
          ab_init,
          ab_mr ++ inc_calc,
          ab_mw ++ calc_end
        ]
      0x47 -> [
          id_init,
          id_sp_mr ++ inc_calc,
          id_sp_mw ++ calc_end
        ]
      0x57 -> [
          id_init,
          id_x_mr ++ inc_calc,
          id_x_mw ++ calc_end
        ]
      0x67 -> [
          aid_init,
          id_x_mr ++ inc_calc,
          id_x_mw ++ calc_end
        ]
      0x77 -> [
          id_init,
          id_y_mr ++ inc_calc,
          id_y_mw ++ calc_end
        ]
      0x87 -> [
          aid_init,
          id_y_mr ++ inc_calc,
          id_y_mw ++ calc_end
        ]
      -- JMP
      0x33 -> [[ld_pc, mr, nf]]
      0x53 -> [
          id_init,
          oe_x:add_calc,
          ld_pc:calc_end
        ]
      0x63 -> [
          aid_init,
          oe_x:add_calc,
          ld_pc:calc_end
        ]
      0x73 -> [
          id_init,
          oe_y:add_calc,
          ld_pc:calc_end
        ]
      0x83 -> [
          aid_init,
          oe_y:add_calc,
          ld_pc:calc_end
        ]
      -- JSR
      0x34 -> [
          dec_sp:f3:f0:(im_init ld_r),
          oe_pc:id_sp_mw,
          ld_pc:calc_end
        ]
      0x54 -> [
          dec_sp:id_init,
          oe_pc:id_sp_mw,
          oe_x:add_calc,
          ld_pc:calc_end
        ]
      0x64 -> [
          dec_sp:aid_init,
          oe_pc:id_sp_mw,
          oe_x:add_calc,
          ld_pc:calc_end
        ]
      0x74 -> [
          dec_sp:id_init,
          oe_pc:id_sp_mw,
          oe_y:add_calc,
          ld_pc:calc_end
        ]
      0x84 -> [
          dec_sp:aid_init,
          oe_pc:id_sp_mw,
          oe_y:add_calc,
          ld_pc:calc_end
        ]
      -- LD
      0xf0 -> [ld_reg_im ld_a]
      0xf1 -> [
          ab_init,
          ld_reg_ab ld_a
        ]
      0xf2 -> [
          id_init,
          ld_reg_ns ld_a
        ]
      0xf3 -> [
          id_init,
          ld_reg_nx ld_a
        ]
      0xf4 -> [
          aid_init,
          ld_reg_ax ld_a
        ]
      0xf5 -> [
          ld_reg_nx' ld_a,
          oe_x:inc_calc',
          ld_x:calc_end
        ]
      0xf6 -> [
          ld_reg_nx' ld_a,
          oe_x:dec_calc',
          ld_x:calc_end
        ]
      0xf7 -> [
          oe_x:inc_calc',
          ld_x:calc_end',
          ld_reg_nx ld_a
        ]
      0xf8 -> [
          oe_x:dec_calc',
          ld_x:calc_end,
          ld_reg_nx ld_a
        ]
      0xf9 -> [
          id_init,
          ld_reg_ny ld_a
        ]
      0xfa -> [
          aid_init,
          ld_reg_ay ld_a
        ]
      0xfb -> [
          ld_reg_ny' ld_a,
          oe_y:inc_calc',
          ld_y:calc_end
        ]
      0xfc -> [
          ld_reg_ny' ld_a,
          oe_y:dec_calc',
          ld_y:calc_end
        ]
      0xfd -> [
          oe_y:inc_calc',
          ld_y:calc_end',
          ld_reg_ny ld_a
        ]
      0xfe -> [
          oe_y:dec_calc',
          ld_y:calc_end,
          ld_reg_ny ld_a
        ]
      0x90 -> [ld_reg_im ld_x]
      0xa0 -> [
          ab_init,
          ld_reg_ab ld_x
        ]
      0xb0 -> [
          id_init,
          ld_reg_ns ld_x
        ]
      0xc0 -> [
          id_init,
          ld_reg_nx ld_x
        ]
      0xd0 -> [
          id_init,
          ld_reg_ny ld_x
        ]
      0x91 -> [ld_reg_im ld_y]
      0xa1 -> [
          ab_init,
          ld_reg_ab ld_y
        ]
      0xb1 -> [
          id_init,
          ld_reg_ns ld_y
        ]
      0xc1 -> [
          id_init,
          ld_reg_nx ld_y
        ]
      0xd1 -> [
          id_init,
          ld_reg_ny ld_y
        ]
      0x92 -> [ld_reg_im ld_sp]
      0xa2 -> [
          ab_init,
          ld_reg_ab ld_sp
        ]
      0xb2 -> [
          id_init,
          ld_reg_ns ld_sp
        ]
      0xc2 -> [
          id_init,
          ld_reg_nx ld_sp
        ]
      0xd2 -> [
          id_init,
          ld_reg_ny ld_sp
        ]
      -- LEA
      0xcc -> [
          id_init,
          oe_x:add_calc,
          ld_x:calc_end
        ]
      0xdc -> [
          id_init,
          oe_sp:add_calc,
          ld_x:calc_end
        ]
      0xcd -> [
          id_init,
          oe_y:add_calc,
          ld_y:calc_end
        ]
      0xdd -> [
          id_init,
          oe_sp:add_calc,
          ld_y:calc_end
        ]
      0xbe -> [
          id_init,
          oe_sp:add_calc,
          ld_sp:calc_end
        ]
      0xce -> [
          id_init,
          oe_x:add_calc,
          ld_sp:calc_end
        ]
      0xde -> [
          id_init,
          oe_y:add_calc,
          ld_sp:calc_end
        ]
      -- LSL (=ASL)
      -- LSR
      0x0c -> [
          oe_a:lsr_calc,
          calc_end_a
        ]
      0x3c -> [
          ab_init,
          ab_mr ++ lsr_calc,
          ab_mw ++ calc_end
        ]
      0x4c -> [
          id_init,
          id_sp_mr ++ lsr_calc,
          id_sp_mw ++ calc_end
        ]
      0x5c -> [
          id_init,
          id_x_mr ++ lsr_calc,
          id_x_mw ++ calc_end
        ]
      0x6c -> [
          aid_init,
          id_x_mr ++ lsr_calc,
          id_x_mw ++ calc_end
        ]
      0x7c -> [
          id_init,
          id_y_mr ++ lsr_calc,
          id_y_mw ++ calc_end
        ]
      0x8c -> [
          aid_init,
          id_y_mr ++ lsr_calc,
          id_y_mw ++ calc_end
        ]
      -- NEG
      0x06 -> [
          oe_a:neg_calc,
          calc_end_a
        ]
      0x36 -> [
          ab_init,
          ab_mr ++ neg_calc,
          ab_mw ++ calc_end
        ]
      0x46 -> [
          id_init,
          id_sp_mr ++ neg_calc,
          id_sp_mw ++ calc_end
        ]
      0x56 -> [
          id_init,
          id_x_mr ++ neg_calc,
          id_x_mw ++ calc_end
        ]
      0x66 -> [
          aid_init,
          id_x_mr ++ neg_calc,
          id_x_mw ++ calc_end
        ]
      0x76 -> [
          id_init,
          id_y_mr ++ neg_calc,
          id_y_mw ++ calc_end
        ]
      0x86 -> [
          aid_init,
          id_y_mr ++ neg_calc,
          id_y_mw ++ calc_end
        ]
      -- NOP
      0x00 -> [nop]
      -- ORA
      0x9a -> [
          im_init ld_t,
          ora_calc,
          calc_end_a
        ]
      0xaa -> [
          ab_init,
          ld_t:ab_mr,
          ora_calc,
          calc_end_a
        ]
      0xba -> [
          id_init,
          ld_t:id_sp_mr,
          ora_calc,
          calc_end_a
        ]
      0xca -> [
          id_init,
          ld_t:id_x_mr,
          ora_calc,
          calc_end_a
        ]
      0xda -> [
          id_init,
          ld_t:id_y_mr,
          ora_calc,
          calc_end_a
        ]
      -- ORCC
      0x02 -> [
          im_init ld_t,
          bitwise_or oe_cc,
          cc_rebind' ++ calc_end
        ]
      -- PSH
      0x10 -> psh_reg oe_a
      0x11 -> psh_reg oe_x
      0x12 -> psh_reg oe_y
      0x13 -> psh_reg oe_cc
      -- PUL
      0x14 -> pul_reg ld_a
      0x15 -> pul_reg ld_x
      0x16 -> pul_reg ld_y
      0x17 -> pul_reg ld_cc
      -- ROL
      0x0d -> [
          oe_a:rol_calc,
          calc_end_a
        ]
      0x3d -> [
          ab_init,
          ab_mr ++ rol_calc,
          ab_mw ++ calc_end
        ]
      0x4d -> [
          id_init,
          id_sp_mr ++ rol_calc,
          id_sp_mw ++ calc_end
        ]
      0x5d -> [
          id_init,
          id_x_mr ++ rol_calc,
          id_x_mw ++ calc_end
        ]
      0x6d -> [
          aid_init,
          id_x_mr ++ rol_calc,
          id_x_mw ++ calc_end
        ]
      0x7d -> [
          id_init,
          id_y_mr ++ rol_calc,
          id_y_mw ++ calc_end
        ]
      0x8d -> [
          aid_init,
          id_y_mr ++ rol_calc,
          id_y_mw ++ calc_end
        ]
      -- ROR
      0x0e -> [
          oe_a:ror_calc,
          calc_end_a
        ]
      0x3e -> [
          ab_init,
          ab_mr ++ ror_calc,
          ab_mw ++ calc_end
        ]
      0x4e -> [
          id_init,
          id_sp_mr ++ ror_calc,
          id_sp_mw ++ calc_end
        ]
      0x5e -> [
          id_init,
          id_x_mr ++ ror_calc,
          id_x_mw ++ calc_end
        ]
      0x6e -> [
          aid_init,
          id_x_mr ++ ror_calc,
          id_x_mw ++ calc_end
        ]
      0x7e -> [
          id_init,
          id_y_mr ++ ror_calc,
          id_y_mw ++ calc_end
        ]
      0x8e -> [
          aid_init,
          id_y_mr ++ ror_calc,
          id_y_mw ++ calc_end
        ]
      -- RTS
      0x43 -> [[inc_sp, mr, g12, ld_pc, nf]]
      -- RTI
      0x44 -> [
          [mr, g12, inc_sp] ++ cc_rebind',
          [mr, g12, inc_sp, ld_a],
          [mr, g12, inc_sp, ld_x],
          [mr, g12, inc_sp, ld_y],
          [mr, g12, inc_sp, ld_pc, nf]
        ]
      -- SBCA
      0x93 -> [
          im_init ld_t,
          sbca_calc,
          calc_end_a
        ]
      0xa3 -> [
          ab_init,
          ld_t:ab_mr,
          sbca_calc,
          calc_end_a
        ]
      0xb3 -> [
          id_init,
          ld_t:id_sp_mr,
          sbca_calc,
          calc_end_a
        ]
      0xc3 -> [
          id_init,
          ld_t:id_x_mr,
          sbca_calc,
          calc_end_a
        ]
      0xd3 -> [
          id_init,
          ld_t:id_y_mr,
          sbca_calc,
          calc_end_a
        ]
      -- ST
      0xe1 -> [
          ab_init,
          st_reg_ab oe_a
        ]
      0xe2 -> [
          id_init,
          st_reg_ns oe_a
        ]
      0xe3 -> [
          id_init,
          st_reg_nx oe_a
        ]
      0xe4 -> [
          aid_init,
          st_reg_ax oe_a
        ]
      0xe5 -> [
          st_reg_nx' oe_a,
          oe_x:inc_calc',
          ld_x:calc_end
        ]
      0xe6 -> [
          st_reg_nx' oe_a,
          oe_x:dec_calc',
          ld_x:calc_end
        ]
      0xe7 -> [
          oe_x:inc_calc',
          ld_x:calc_end',
          st_reg_nx oe_a
        ]
      0xe8 -> [
          oe_x:dec_calc',
          ld_x:calc_end,
          st_reg_nx oe_a
        ]
      0xe9 -> [
          id_init,
          st_reg_ny oe_a
        ]
      0xea -> [
          aid_init,
          st_reg_ay oe_a
        ]
      0xeb -> [
          st_reg_ny' oe_a,
          oe_y:inc_calc',
          ld_y:calc_end
        ]
      0xec -> [
          st_reg_ny' oe_a,
          oe_y:dec_calc',
          ld_y:calc_end
        ]
      0xed -> [
          oe_y:inc_calc',
          ld_y:calc_end',
          st_reg_ny oe_a
        ]
      0xee -> [
          oe_y:dec_calc',
          ld_y:calc_end,
          st_reg_ny oe_a
        ]
      0x30 -> [
          ab_init,
          st_reg_ab oe_x
        ]
      0x40 -> [
          id_init,
          st_reg_ns oe_x
        ]
      0x50 -> [
          id_init,
          st_reg_nx oe_x
        ]
      0x60 -> [
          aid_init,
          st_reg_ax oe_x
        ]
      0x70 -> [
          id_init,
          st_reg_ny oe_x
        ]
      0x80 -> [
          aid_init,
          st_reg_ay oe_x
        ]
      0x31 -> [
          ab_init,
          st_reg_ab oe_y
        ]
      0x41 -> [
          id_init,
          st_reg_ns oe_y
        ]
      0x51 -> [
          id_init,
          st_reg_nx oe_y
        ]
      0x61 -> [
          aid_init,
          st_reg_ax oe_y
        ]
      0x71 -> [
          id_init,
          st_reg_ny oe_y
        ]
      0x81 -> [
          aid_init,
          st_reg_ay oe_y
        ]
      0x32 -> [
          ab_init,
          st_reg_ab oe_sp
        ]
      0x42 -> [
          id_init,
          st_reg_ns oe_sp
        ]
      0x52 -> [
          id_init,
          st_reg_nx oe_sp
        ]
      0x62 -> [
          aid_init,
          st_reg_ax oe_sp
        ]
      0x72 -> [
          id_init,
          st_reg_ny oe_sp
        ]
      0x82 -> [
          aid_init,
          st_reg_ay oe_sp
        ]
      -- SUBA
      0x94 -> [
          im_init ld_t,
          suba_calc,
          calc_end_a
        ]
      0xa4 -> [
          ab_init,
          ld_t:ab_mr,
          suba_calc,
          calc_end_a
        ]
      0xb4 -> [
          id_init,
          ld_t:id_sp_mr,
          suba_calc,
          calc_end_a
        ]
      0xc4 -> [
          id_init,
          ld_t:id_x_mr,
          suba_calc,
          calc_end_a
        ]
      0xd4 -> [
          id_init,
          ld_t:id_y_mr,
          suba_calc,
          calc_end_a
        ]
      -- TFR
      0x18 -> tfr_regs oe_a ld_cc
      0x19 -> tfr_regs oe_cc ld_a
      0x1a -> tfr_regs oe_x ld_y
      0x1b -> tfr_regs oe_y ld_x
      0x1c -> tfr_regs oe_x ld_sp
      0x1d -> tfr_regs oe_sp ld_x
      0x1e -> tfr_regs oe_y ld_sp
      0x1f -> tfr_regs oe_sp ld_y
      -- TST
      0x09 -> [oe_a:tst_calc_end]
      0x39 -> [
          ab_init,
          ab_mr ++ tst_calc_end
        ]
      0x49 -> [
          id_init,
          id_sp_mr ++ tst_calc_end
        ]
      0x59 -> [
          id_init,
          id_x_mr ++ tst_calc_end
        ]
      0x69 -> [
          aid_init,
          id_x_mr ++ tst_calc_end
        ]
      0x79 -> [
          id_init,
          id_y_mr ++ tst_calc_end
        ]
      0x89 -> [
          aid_init,
          id_y_mr ++ tst_calc_end
        ]
      -- Undefined
      _ -> sw_interrupt
