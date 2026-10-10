module Output (runFlisp) where

import System.IO (hFlush, stdout)

import Numeric (showHex)

import Flisp
import qualified Flisp.Bit as Bit
import qualified Flisp.Byte as Byte

data NumberMode = Dec | Bin | Hex deriving (Eq)

data OutputState = OutputState {
  flisp :: Flisp,
  numberMode :: NumberMode
}

showBit :: Bit.Bit -> String
showBit bit =
  let
    c = if bit == Bit.One
        then "X"
        else " "
  in
    "[" ++ c ++ "]"

showByte :: Byte.Byte -> NumberMode -> String
showByte byte Dec = show (Byte.toInt byte)
showByte byte Bin = show byte
showByte byte Hex =
  let
    hex' = showHex (Byte.toInt byte) ""
    hex = if length hex' == 1
          then "0" ++ hex'
          else hex'
  in
    "0x" ++ hex

instance Show OutputState where
  show (OutputState {
    flisp = flisp@(Flisp.ControlUnit {
      counter = (Flisp.Counter { q3, q2, q1, q0 }),
      datapath = (Flisp.Datapath { reg_a })
    }),
    numberMode
  }) =
    let
      (nf, inputs) = controlSignals flisp
    in
      show inputs ++ "\n\n" ++ show flisp

parseInput :: String -> OutputState -> OutputState
parseInput input (OutputState {
  flisp,
  numberMode
}) = OutputState {
  flisp =
    let
      value
        | null input       = clock flisp
        | 'r' `elem` input = reset flisp
        | otherwise        = flisp
    in
      value,
  numberMode =
    let
      value
        | 'd' `elem` input = Dec
        | 'b' `elem` input = Bin
        | 'h' `elem` input = Hex
        | otherwise        = numberMode
    in
      value
}

runFlisp :: Flisp -> IO ()
runFlisp flisp = do
  let state = OutputState {
    flisp = flisp,
    numberMode = Hex
  }
  loop state

loop :: OutputState -> IO ()
loop state = do
  print state

  putStr "\n>>> "
  hFlush stdout
  input <- getLine
  putStrLn "\n"

  let newState = parseInput input state

  if input /= "q"
  then loop newState
  else return ()
