module Types.Memory where

import Types.Byte as Byte
import Types.Bit as Bit
import Types.Bus as Bus

type Address = Byte.Byte
type Memory = [Byte.Byte]

size :: Int
size = 256

zero :: Memory
zero = replicate size Byte.zero :: Memory

read :: Memory -> Address -> Bit -> Bus.Bus
read _ _ Bit.Zero = Nothing
read memory address Bit.One =
  let
    index = Byte.toInt address
  in
    -- Index can't be outside the range of 0-255
    Just $ memory !! index

write :: Memory -> Address -> Bit -> Byte -> Memory
write memory _ Bit.Zero _ = memory
write memory address Bit.One input =
  let
    index = Byte.toInt address
    indexed_list = zip [0..size] memory
  in
    foldr (\ (i, cur) acc -> (if i == index then input else cur):acc) [] indexed_list

fromList :: [Byte.Byte] -> Memory
fromList list =
  if length list == size
  then list
  else replicate size Byte.zero :: Memory
