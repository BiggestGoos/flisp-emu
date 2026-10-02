{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Types.Bit.Operations where

import Prelude ((==), (&&), (||), ($), (++), foldr, zip, reverse)
import Types.Bit.Type

-- Logic

-- AND
and :: Bit -> Bit -> Bit
and l r = if l == One && r == One then One else Zero

(.) :: Bit -> Bit -> Bit
(.) = Types.Bit.Operations.and

-- AND n-bits
andn :: [Bit] -> Bit
andn [] = One
andn (x:xs) = x . (andn xs)

-- AND block
andb :: [Bit] -> [Bit] -> [Bit]
andb _ [] = []
andb [] _ = []
andb (l:ls) (r:rs) = (l . r):(andb ls rs)

-- OR
or :: Bit -> Bit -> Bit
or l r = if l == One || r == One then One else Zero

(+) :: Bit -> Bit -> Bit
(+) = Types.Bit.Operations.or

-- OR n-bits
orn :: [Bit] -> Bit
orn [] = Zero
orn (x:xs) = x + (orn xs)

-- OR block
orb :: [Bit] -> [Bit] -> [Bit]
orb _ [] = []
orb [] _ = []
orb (l:ls) (r:rs) = (l + r):(orb ls rs)

-- NOT
not :: Bit -> Bit
not bit = if bit == Zero then One else Zero

-- NOT block
notb :: [Bit] -> [Bit]
notb [] = []
notb (x:xs) = (not x):(notb xs)

-- NAND
nand :: Bit -> Bit -> Bit
nand l r = not $ and l r

-- NAND n-bits
nandn :: [Bit] -> Bit
nandn xs = not $ andn xs

-- NAND block
nandb :: [Bit] -> [Bit] -> [Bit]
nandb _ [] = []
nandb [] _ = []
nandb (l:ls) (r:rs) = (l `nand` r):(nandb ls rs)

-- NOR
nor :: Bit -> Bit -> Bit
nor l r = not $ and l r

-- NOR n-bits
norn :: [Bit] -> Bit
norn xs = not $ orn xs

-- NOR block
norb :: [Bit] -> [Bit] -> [Bit]
norb _ [] = []
norb [] _ = []
norb (l:ls) (r:rs) = (l `nor` r):(norb ls rs)

-- XOR
xor :: Bit -> Bit -> Bit
xor l r = ((not l) . r) + (l . (not r))

-- XOR n-bits
xorn :: [Bit] -> Bit
xorn [] = Zero
xorn (x:xs) = x `xor` (xorn xs)

-- XOR block
xorb :: [Bit] -> [Bit] -> [Bit]
xorb _ [] = []
xorb [] _ = []
xorb (l:ls) (r:rs) = (l `xor` r):(xorb ls rs)

-- XNOR
xnor :: Bit -> Bit -> Bit
xnor l r = not $ xor l r

-- XNOR n-bit
xnorn :: [Bit] -> Bit
xnorn xs = not $ xorn xs

-- XNOR block
xnorb :: [Bit] -> [Bit] -> [Bit]
xnorb _ [] = []
xnorb [] _ = []
xnorb (l:ls) (r:rs) = (l `xnor` r):(xnorb ls rs)

-- Arithemtic

type Carry = Bit
fullAdder :: Bit -> Bit -> Carry -> (Bit, Carry)
fullAdder x y cin =
  let
    s_im = x `xor` y
  in
  (
    s_im `xor` cin,
    (x . y) + (cin . s_im)
  )

fullAdderBits :: [Bit] -> [Bit] -> Carry -> ([Bit], Carry)
fullAdderBits xs ys cin =
  let
    (sum, carry) = foldr (\ (x, y) (sum, carry) ->
      let
        (newSum, newCarry) = fullAdder x y carry
      in
        (sum ++ [newSum], newCarry)) ([], cin) (zip xs ys)
  in
    (reverse sum, carry)
