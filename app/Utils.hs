module Utils where

import Data.Char (toLower)

hexToInt :: Char -> Int
hexToInt c =
  case toLower c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'a' -> 10
    'b' -> 11
    'c' -> 12
    'd' -> 13
    'e' -> 14
    'f' -> 15
    _   -> error $ [c] ++ " is not a hexadecimal digit!"

twoHexToInt :: Char -> Char -> Int
twoHexToInt l r = (16 * hexToInt l) + hexToInt r

hexBytesToInts :: [Char] -> [Int]
hexBytesToInts [] = []
hexBytesToInts (_:[]) = error "Can't convert a list with an odd amount of chars to ints"
hexBytesToInts (l:r:rest) = (twoHexToInt l r):(hexBytesToInts rest)
