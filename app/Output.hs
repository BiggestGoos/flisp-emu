module Output (runFlisp) where

import System.IO (hFlush, stdout)
import Data.List (stripPrefix, isPrefixOf)
import Numeric (showHex)

import Flisp
import qualified Flisp.Bit as Bit
import qualified Flisp.Byte as Byte

import Utils

data NumberMode = Dec | Bin | Hex deriving (Eq)

data OutputState = OutputState {
  flisp :: Flisp,
  numberMode :: NumberMode,
  showSignals :: Bool,
  breakpoint :: Maybe Byte.Byte,
  run :: Maybe Int, -- If Nothing, then don't run, if 0 run full speed, if n, run at n Hz (TODO last part)
  inputs :: (Byte.Byte, Byte.Byte)
}

testBreakpoint :: OutputState -> (Bool, OutputState)
testBreakpoint state@(OutputState {
  flisp = flisp@(Flisp.ControlUnit {
    counter,
    datapath = (Flisp.Datapath {
      reg_pc
    })
  }),
  numberMode,
  showSignals,
  breakpoint,
  inputs
}) =
  if
    let
      bp = case breakpoint of
        Nothing -> Byte.zero -- Can't happen
        (Just bp') -> bp'
      fetch = Flisp.Counter Bit.Zero Bit.Zero Bit.One Bit.One
    in
      breakpoint /= Nothing && reg_pc == bp && counter == fetch
  then (True, OutputState {
    flisp,
    numberMode,
    showSignals,
    breakpoint,
    run = Nothing,
    inputs
  })
  else (False, state)

showBit :: Bit.Bit -> String
showBit bit =
  let
    c = if bit == Bit.One
        then "X"
        else " "
  in
    "[" ++ c ++ "]"

showByte :: Byte.Byte -> NumberMode -> String
showByte byte Dec =
  let
    dec' = show (Byte.toInt byte)
    missing = 3 - (length dec')
    dec = (replicate missing '0') ++ dec'
  in
    dec
showByte byte Bin = show byte
showByte byte Hex =
  let
    hex' = showHex (Byte.toInt byte) ""
    hex = if length hex' == 1
          then "0" ++ hex'
          else hex'
  in
    "0x" ++ hex

showRegister :: Byte.Byte -> String -> NumberMode -> String
showRegister byte label numbermode = label ++ ": " ++ (showByte byte numbermode)

showMemory :: Flisp.Memory -> Byte.Byte -> Byte.Byte -> NumberMode -> String
showMemory memory pc sp numbermode =
  let
    pc_index = Byte.toInt pc
    sp_index = Byte.toInt sp
    indexed = zip [0..255] memory
    per_row = 8
  in
    foldl' (\ acc (index, byte) ->
      let
        spacing =
          if index == pc_index && index == sp_index
          then ('{', '}')
          else
            if index == pc_index
            then ('[', ']')
            else
              if index == sp_index
              then ('(', ')')
              else (' ', ' ')
        new_line =
          if index `mod` per_row /= 0
          then ""
          else "\n (" ++ (showByte (Byte.fromInt index) Hex) ++ ") "
      in
        acc ++ new_line ++ [fst spacing] ++ (showByte byte numbermode) ++ [snd spacing]
    ) "" indexed

showDatapath :: Flisp.Datapath -> NumberMode -> String
showDatapath (Flisp.Datapath {
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
}) numbermode =
  let
    registers_top = [(reg_a, "Reg A"), (reg_t, "Reg  T"), (reg_x, "Reg  X"), (reg_y, "Reg  Y"), (reg_ta, "Reg TA")]
    registers_bot = [(reg_r, "Reg R"), (reg_cc, "Reg CC"), (reg_sp, "Reg SP"), (reg_pc, "Reg PC"), (reg_i, "Reg  I")]
    top_string = (foldl' (\ acc (byte, label) -> acc ++ " --- " ++ (showRegister byte label numbermode)) "" registers_top) ++ " ---"
    bot_string = (foldl' (\ acc (byte, label) -> acc ++ " --- " ++ (showRegister byte label numbermode)) "" registers_bot) ++ " ---"
  in
    top_string ++ "\n" ++ bot_string ++ "\n\n" ++ (showMemory memory reg_pc reg_sp numbermode)

showCounter :: Flisp.Counter -> String
showCounter (Flisp.Counter q3 q2 q1 q0) = "Counter: Q" ++ (show (Bit.bitsToInt [q3, q2, q1, q0]))

showSignals' :: Flisp -> String
showSignals' flisp =
  let
    (nf, (Flisp.Inputs {
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
    }), irq) = controlSignals flisp

    signalsToString signals = drop 2 (foldl' (\ acc (label, bit) -> acc ++ ", " ++ label ++ ": " ++ (showBit bit)) "" signals)

    load_signals = [("A", ld_a), ("T", ld_t), ("X", ld_x), ("Y", ld_y), ("PC", ld_pc), ("SP", ld_sp), ("TA", ld_ta), ("R", ld_r), ("CC", ld_cc), ("I", ld_i)]
    load_line = "Load: " ++ (signalsToString load_signals) ++ "\n"
    open_signals = [("A", oe_a), ("X", oe_x), ("Y", oe_y), ("PC", oe_pc), ("SP", oe_sp), ("R", oe_r), ("CC", oe_cc)]
    open_line = "Open: " ++ (signalsToString open_signals) ++ "\n"
    clr_signals = [("T", clr_t)]
    clr_line = "Clear: " ++ (signalsToString clr_signals) ++ "\n"
    inc_signals = [("PC", inc_pc), ("SP", inc_sp)]
    inc_line = "Increase: " ++ (signalsToString inc_signals) ++ "\n"
    dec_signals = [("SP", dec_sp)]
    dec_line = "Decrease: " ++ (signalsToString dec_signals) ++ "\n"
    alu_signals = [("3", f3), ("2", f2), ("1", f1), ("0", f0)]
    alu_line = "ALU Function: " ++ (signalsToString alu_signals) ++ "\n"
    mem_signals = [("Read", mr), ("Write", mw)]
    mem_line = "Memory: " ++ (signalsToString mem_signals) ++ "\n"
    mux_signals = [("14", g14), ("13", g13), ("12", g12), ("11", g11), ("10", g10), ("9", g9), ("8", g8), ("7", g7), ("6", g6), ("5", g5), ("4", g4), ("3", g3), ("2", g2), ("1", g1), ("0", g0)]
    mux_line = "gX: " ++ (signalsToString mux_signals) ++ "\n"
    nf_line = "NF: " ++ (showBit nf) ++ "\n"
    irq' = case irq of
      Nothing    -> "[+]"
      (Just bit) -> showBit bit
    irq_line = "IRQ: " ++ irq' ++ "\n"

  in
    "Signals: \n" ++ load_line ++ open_line ++ clr_line ++ inc_line ++ dec_line ++ alu_line ++ mem_line ++ mux_line ++ nf_line ++ irq_line

showInputs :: (Byte.Byte, Byte.Byte) -> NumberMode -> String
showInputs (inputFB, inputFC) nrMode = "Inputs\nFB: " ++ (showByte inputFB nrMode) ++ "\nFC: " ++ (showByte inputFC nrMode) ++ "\n"

instance Show OutputState where
  show (OutputState {
    flisp = flisp@(Flisp.ControlUnit {
      counter,
      datapath
    }),
    numberMode,
    showSignals,
    breakpoint,
    inputs
  }) =
    let
      signals =
        if showSignals
        then (showSignals' flisp) ++ "\n"
        else ""
      bp = (case breakpoint of
        Nothing -> ""
        (Just bp') -> "Breakpoint: " ++ (showByte bp' Hex)) ++ "\n"
    in
       signals ++ bp ++ (showInputs inputs numberMode) ++ "\n" ++ (showCounter counter) ++ "\n" ++ (showDatapath datapath numberMode)

parseData :: Maybe String -> Maybe (Int, Int)
parseData (Just (adr2:adr1:'$':data2:data1:[])) =
  let
    adr = twoHexToInt adr2 adr1
    input = twoHexToInt data2 data1
  in
    Just (adr, input)
parseData (Just (adr2:adr1:'%':bits')) =
  let
    bits = map (\ digit ->
      if digit == '0'
      then Bit.Zero
      else (
        if digit == '1'
        then Bit.One
        else error $ [digit] ++ " is not a valid bit for binary numbers!"
      )) bits'
    adr = twoHexToInt adr2 adr1
    input = Bit.bitsToInt bits
  in
    Just (adr, input)
parseData (Just (_:_:[])) = Nothing
parseData (Just (adr2:adr1:digits')) =
  let
    adr = twoHexToInt adr2 adr1
    realDigits = ['0'..'9']
    digits = map (\ x -> if not $ x `elem` realDigits then error $ [x] ++ " is not a valid digit for decimal numbers!" else x) digits'
    input = read digits
  in
    Just (adr, input)
parseData _ = Nothing

setMemory :: Maybe String -> Flisp -> Flisp
setMemory Nothing flisp = flisp
setMemory inputData flisp =
  let
    parsedData = parseData inputData
    (adr, input) = case parsedData of
      Just data' -> data'
      Nothing -> (0, 0)
  in
    if parsedData == Nothing
    then flisp
    else write input adr flisp

parseInput :: String -> OutputState -> OutputState
parseInput input (OutputState {
  flisp,
  numberMode,
  showSignals,
  breakpoint,
  inputs = inputs@(inputFB, inputFC)
}) = OutputState {
  flisp =
    let
      value
        | null input = clock' flisp inputs
        | input == "r" = reset flisp
        | input == "irq" = interruptRequest flisp
        | "mw" `isPrefixOf` input = setMemory (stripPrefix "mw" input) flisp
        | otherwise        = flisp
    in
      value,
  numberMode =
    let
      value
        | input == "d" = Dec
        | input == "b" = Bin
        | input == "h" = Hex
        | otherwise        = numberMode
    in
      value,
  showSignals =
    if input == "s"
    then not showSignals
    else showSignals,
  breakpoint =
    if "bp" `isPrefixOf` input
    then
      let
        bp = case input of
          'b':'p':adr1:adr0:[] -> Just $ Byte.fromInt $ twoHexToInt adr1 adr0
          _ -> Nothing
      in
        bp
    else breakpoint,
  run =
    if "run" `isPrefixOf` input
    then Just 0
    else Nothing,
  inputs =
    let
      parsedData =
        if "in" `isPrefixOf` input
        then parseData $ stripPrefix "in" input
        else Nothing
      (adr, newInput) = case parsedData of
        Just data' -> data'
        Nothing -> (0, 0)
    in (
      if adr == 0xfb
      then Byte.fromInt newInput
      else inputFB,
      if adr == 0xfc
      then Byte.fromInt newInput
      else inputFC
    )
}

runFlisp :: Flisp -> IO ()
runFlisp flisp = do
  let state = OutputState {
    flisp = flisp,
    numberMode = Hex,
    showSignals = False,
    breakpoint = Nothing,
    run = Nothing,
    inputs = (Byte.zero, Byte.zero)
  }
  loop state

prompt :: IO String
prompt = do
  putStr "\n>>> "
  hFlush stdout
  input <- getLine
  putStrLn "\n"
  return input

loop :: OutputState -> IO ()
loop state@(OutputState {
  flisp,
  numberMode,
  showSignals,
  breakpoint,
  inputs
}) = do
  print state

  let (bp_hit, bp_state@(OutputState {
    run
  })) = testBreakpoint state

  if bp_hit
  then
    let
      bp_byte = case breakpoint of
        Nothing -> Byte.zero -- Can't happen
        (Just bp) -> bp
    in
       putStrLn $ "Breakpoint hit at: " ++ (showByte bp_byte Hex)
  else return ()

  if run == Nothing
  then do
    input <- prompt

    let newState = parseInput input bp_state

    if input /= "q"
    then loop newState
    else return ()
  else loop (OutputState {
    flisp = clock' flisp inputs,
    numberMode,
    showSignals,
    breakpoint,
    run,
    inputs
  })
