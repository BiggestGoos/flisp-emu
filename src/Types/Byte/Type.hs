module Types.Byte.Type where

import Types.Bit

data Byte = Byte Bit Bit Bit Bit Bit Bit Bit Bit deriving (Ord, Eq)

instance Show Byte where
  show byte = foldl (\ acc cur -> acc ++ (show cur)) "" (bits byte)

zero :: Byte
zero = Byte Zero Zero Zero Zero Zero Zero Zero Zero

full :: Byte
full = Byte One One One One One One One One

bits :: Byte -> [Bit]
bits (Byte a b c d e f g h) = [a,b,c,d,e,f,g,h]

-- Converts a list of n bits (n >= 8) to a byte, bits past the 8th are discarded
unbits :: [Bit] -> Byte
unbits (a:b:c:d:e:f:g:h:_) = Byte a b c d e f g h
unbits _ = error "Only lists of bits with length 8 or more can be converted to bytes!"

fromInt :: Int -> Byte
fromInt int = unbits $ bitsFromInt int 8

toInt :: Byte -> Int
toInt byte = bitsToInt $ bits byte
