{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators,
             FlexibleInstances, FlexibleContexts, UndecidableInstances,
             EmptyDataDecls #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel.Num.Ops
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTC, non-standard instances)
--
-- Type-level numerical operations and its value-level reflection functions.
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Num.Ops 
 (-- * Successor/Predecessor
  Succ, succRef,
  Pred, predRef,
  -- * Addition/Subtraction
  Add, addRef,
  Sub, subRef,
  -- * Multiplication/Division
  Mul, mulRef,
  Div, divRef,
  -- * Comparison assertions
  -- ** General comparison assertion
  Compare,
  -- *** Type-level values denoting comparison results
  CLT, CEQ, CGT,
  -- ** Abbreviated comparison assertions
  (:==:), (:>:), (:<:), (:>=:), (:<=:) 
  ) where

import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Sets


-------------------------
-- Successor, Predecessor
-------------------------

-- | Successor type-level relation
class (Nat x, Pos y) => Succ x y | x -> y, y -> x


instance (Pos y, NClassify y yc, DivMod10 xh xl x, Succ' xh xl yh yl yc,
         DivMod10 yh yl y)
   => Succ x y

class Succ' xh xl yh yl yc | xh xl -> yh yl yc, yh yl yc -> xh xl


-- This intends to implement a user reporting operation when
-- trying to calculate the predecesor of 0
-- FIXME: however, the instance rule is never triggered!

class Failure t
-- No instances
data PredecessorOfZeroError t
 
instance Failure (PredecessorOfZeroError x) => Succ' (x,x) (x,x) D0 D0 Z
instance Succ' xh D0 xh D1 P
instance Succ' xh D1 xh D2 P
instance Succ' xh D2 xh D3 P
instance Succ' xh D3 xh D4 P
instance Succ' xh D4 xh D5 P
instance Succ' xh D5 xh D6 P
instance Succ' xh D6 xh D7 P
instance Succ' xh D7 xh D8 P
instance Succ' xh D8 xh D9 P
instance Succ xh yh => Succ' xh D9 yh D0 P


{-

Nicer, but not relational implementation of Succ

class (Nat x, Pos y) => Succ' x y | x -> y

-- by structural induction on the first argument
instance Succ' D0 D1
instance Succ' D1 D2
instance Succ' D2 D3
instance Succ' D3 D4
instance Succ' D4 D5
instance Succ' D5 D6
instance Succ' D6 D7
instance Succ' D7 D8
instance Succ' D8 D9
instance Succ' D9 (D1 :* D0)
instance Pos x => Succ' (x :* D0) (x :* D1)
instance Pos x => Succ' (x :* D1) (x :* D2)
instance Pos x => Succ' (x :* D2) (x :* D3)
instance Pos x => Succ' (x :* D3) (x :* D4)
instance Pos x => Succ' (x :* D4) (x :* D5)
instance Pos x => Succ' (x :* D5) (x :* D6)
instance Pos x => Succ' (x :* D6) (x :* D7)
instance Pos x => Succ' (x :* D7) (x :* D8)
instance Pos x => Succ' (x :* D8) (x :* D9)
instance (Pos x, Succ' x y) => Succ' (x :* D9) (y  :* D0)


class (Nat x, Pos y) => Succ x y | x -> y, y -> x
instance Succ' x y => Succ x y
-}

-- | value-level reflection function for the succesor type-level relation
--   (named succRef to avoid a clash with 'Prelude.succ')
succRef :: Succ x y => x -> y
succRef = undefined

-- Note: maybe redundant 
-- | Predecessor type-level relation
class (Pos x, Nat y) => Pred x y | x -> y, y -> x
instance Succ x y => Pred y x

-- | value-level reflection function for the predecessor type-level relation
--   (named succRef to avoid a clash with 'Prelude.pred')
predRef :: Pred x y => x -> y
predRef = undefined



--------------------
-- Add and Subtract
--------------------


class (Nat x, Nat y, Nat z) => Add' x y z | x y -> z, z x -> y

-- by structural induction on the first argument
instance Nat y   => Add' D0 y y
instance Succ y z => Add' D1 y z
instance (Succ z z', Add' D1 y z) => Add' D2 y z'
instance (Succ z z', Add' D2 y z) => Add' D3 y z'
instance (Succ z z', Add' D3 y z) => Add' D4 y z'
instance (Succ z z', Add' D4 y z) => Add' D5 y z'
instance (Succ z z', Add' D5 y z) => Add' D6 y z'
instance (Succ z z', Add' D6 y z) => Add' D7 y z'
instance (Succ z z', Add' D7 y z) => Add' D8 y z'
instance (Succ z z', Add' D8 y z) => Add' D9 y z'
-- multidigit addition
instance (Pos x, Pos (zi :* zl), Add' x yi zi, DivMod10 yi yl y)
    => Add' (x :* D0) y (zi :* zl)
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Succ (zi :* yl) z)
    => Add' (x :* D1) y z
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Add' D2 (zi :* yl) z)
    => Add' (x :* D2) y z
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Add' D3 (zi :* yl) z)
    => Add' (x :* D3) y z
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Add' D4 (zi :* yl) z)
    => Add' (x :* D4) y z
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Add' D5 (zi :* yl) z)
    => Add' (x :* D5) y z
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Add' D6 (zi :* yl) z)
    => Add' (x :* D6) y z
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Add' D7 (zi :* yl) z)
    => Add' (x :* D7) y z
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Add' D8 (zi :* yl) z)
    => Add' (x :* D8) y z
instance (Pos x, Nat z, Add' x yi zi, DivMod10 yi yl y, Add' D9 (zi :* yl) z)
    => Add' (x :* D9) y z


{-

-- Nicer (but not relational) multidigit addition. 

-- The algortihm follows the one used when 
-- "using pen and paper", not relational :S
-- e.g. 
--   4567
--  +9345      4567 + 9345 =  ((5 + 7) mod 10) + 
--   ----                     ( ((5 + 7) div 10) + (456 + 934) ) * 10  
--  13912 
instance (Pos (xi :* xl),           -- decompose first arg
          Nat y, DivMod10 yi yl y, -- decompose second arg
          Pos (zi' :* fl),
          Add' xl yl f,            -- Add the first two digits of x and y
          DivMod10 fi fl f,        -- Obtain carryout of the previous sum
          Add' xi yi zi,           -- Add the other digits and the carryout 
          Add' zi fi zi') 
  => Add' (xi :* xl) y (zi' :* fl)  
-}

-- | Addition type-level relation
class (Add' x y z, Add' y x z) => Add x y z | x y -> z, z x -> y, z y -> x
instance (Add' x y z, Add' y x z) => Add x y z


-- | value-level reflection function for the addition type-level relation 
addRef :: (Add x y z) => x -> y -> z
addRef = undefined

-- | Subtraction type-level relation
class Sub x y z | x y -> z, z x -> y, z y -> x
instance Add x y z => Sub z y x

-- | value-level reflection function for the addition type-level relation 
subRef :: (Sub x y z) => x -> y -> z
subRef = undefined

------------------------------
-- Multiplication and Division
------------------------------


class (Nat x, Nat y, Nat z) => Mul' x y z | x y -> z, x z -> y

-- By structural induction on the first argument
instance Nat y => Mul' D0 y D0
instance Nat y => Mul' D1 y y
instance Add y y z => Mul' D2 y z
instance (Add z y z', Mul' D2 y z) => Mul' D3 y z'
instance (Add z y z', Mul' D3 y z) => Mul' D4 y z'
instance (Add z y z', Mul' D4 y z) => Mul' D5 y z'
instance (Add z y z', Mul' D5 y z) => Mul' D6 y z'
instance (Add z y z', Mul' D6 y z) => Mul' D7 y z'
instance (Add z y z', Mul' D7 y z) => Mul' D8 y z'
instance (Add z y z', Mul' D8 y z) => Mul' D9 y z'
-- Nice, but not relational multidigit multiplication

-- FIXME: not being relational breaks division!
--
-- The algorithm follows the one used with pen-and-paper
-- e.g. 
--   4567
--  x  45      4567 * 45 =  (5 * 4567)  + (4 * 4567) * 10 
--   ----                     
--  22835
-- 18268
-- ------
-- 205515

instance (Pos (xi :* xl), -- Decompose the first arg (multiplier) 
          Nat y,  -- Multiplicand
          Mul' xl y fr, -- Multiply using the last digit of the multiplier
                        -- obtaining the first row
          Mul' xi y rr, -- Multiply the rest.   
          Add  fr (rr :* D0) z -- Add the rows.
   ) => Mul' (xi :* xl) y z


{-

Note: I started to port this relational multidigit multiplication
      code from Oleg's Binary implementation but is really confusing.

-- We assert that x * y = z, with x > 0
class (Pos x, Nat y, Nat z) => Mul' x y z | x y -> z, x z -> y

-- by structural induction on the first argument
instance Nat y => Mul' D1 y y
instance (Mul' x y zh, DivMod10 zh D0 z) => Mul' (x :* D0) y z

instance (Mul'F x y z,  Mul'B x y z) => Mul' (x :* D1) y z

-- We assert that (2x+1) * y = z with x > 0
class (Pos x, Nat y, Nat z) => Mul'F x y z | x y -> z

instance Pos x => Mul'F x D0 D0
instance Pos x => Mul'F x D1 (x :* D1)

-- (2x+1) * 2y
instance (Mul'F x y z, Pos x, Pos y, Pos z) => Mul'F x (y :* D0) (z :* D0)

-- (2x+1) * (2y+1) = 2*( (2x+1)*y + x ) + 1, y > 0
instance (Mul'F x y z', Add x z' z, Pos x, Pos y, Pos z) 
    => Mul'F x (y :* D1) (z :* D1)

-- We assert that (2x+1) * y = z with x > 0
-- The functional dependencies go the other way though
class (Pos x, Nat y, Nat z) => Mul'B x y z | z x -> y
instance Pos x => Mul'B x D0 D0
-- instance Pos x => Mul'B x y B1 -- cannot happen

-- (2x+1) * 2y
instance (Mul'B x y z, Pos x, Pos y, Pos z) => Mul'B x (y :* D0) (z :* D0)
-- (2x+1) * (2y+1) = 2*( (2x+1)*y + x ) + 1, y >= 0
instance (DivMod10 y D1 yt, Mul'B x y z', Add x z' z, Pos x, Pos z) 
    => Mul'B x yt (z :* D1)
-}

-- | Multiplication type-level relation
class (Mul' x y z, Mul' y x z) => Mul x y z | x y -> z, x z -> y, y z -> x
instance (Mul' x y z, Mul' y x z) => Mul x y z

-- | value-level reflection function for the multiplication type-level relation 
mulRef :: Mul x y z => x -> y -> z
mulRef = undefined

-- | Division type-level relation
--  FIXME: Broken! (due to multiplication not being relational)
class Div x y z | x y -> z, x z -> y, y z -> x
instance (Mul x y z) => Div z y x

-- | value-level reflection function for the division type-level relation 
divRef :: Mul x y z => z -> x -> y
divRef = undefined


-------------
-- Comparison
-------------

-- type-level values denoting comparison results

-- | Lower than 
data CLT
-- | Equal
data CEQ
-- | Greater than
data CGT

-- | General comparison type-level assertion
class (Nat x, Nat y) => Compare x y r | x y -> r


-- by structural induction on the first, and then the second argument
-- D0
instance Compare D0 D0 CEQ
instance Compare D0 D1 CLT
instance Compare D0 D2 CLT
instance Compare D0 D3 CLT
instance Compare D0 D4 CLT
instance Compare D0 D5 CLT
instance Compare D0 D6 CLT
instance Compare D0 D7 CLT
instance Compare D0 D8 CLT
instance Compare D0 D9 CLT
instance Pos (yi :* yl) => Compare D0 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D0 CGT
-- D1
instance Compare D1 D0 CGT
instance Compare D1 D1 CEQ
instance Compare D1 D2 CLT
instance Compare D1 D3 CLT 
instance Compare D1 D4 CLT
instance Compare D1 D5 CLT 
instance Compare D1 D6 CLT
instance Compare D1 D7 CLT 
instance Compare D1 D8 CLT
instance Compare D1 D9 CLT
instance Pos (yi :* yl) => Compare D1 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D1 CGT
-- D2
instance Compare D2 D0 CGT
instance Compare D2 D1 CGT
instance Compare D2 D2 CEQ
instance Compare D2 D3 CLT
instance Compare D2 D4 CLT
instance Compare D2 D5 CLT
instance Compare D2 D6 CLT
instance Compare D2 D7 CLT
instance Compare D2 D8 CLT
instance Compare D2 D9 CLT
instance Pos (yi :* yl) => Compare D2 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D2 CGT
-- D3
instance Compare D3 D0 CGT
instance Compare D3 D1 CGT
instance Compare D3 D2 CGT
instance Compare D3 D3 CEQ
instance Compare D3 D4 CLT
instance Compare D3 D5 CLT
instance Compare D3 D6 CLT
instance Compare D3 D7 CLT
instance Compare D3 D8 CLT
instance Compare D3 D9 CLT
instance Pos (yi :* yl) => Compare D3 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D3 CGT
-- D4
instance Compare D4 D0 CGT
instance Compare D4 D1 CGT
instance Compare D4 D2 CGT
instance Compare D4 D3 CGT
instance Compare D4 D4 CEQ
instance Compare D4 D5 CLT
instance Compare D4 D6 CLT
instance Compare D4 D7 CLT
instance Compare D4 D8 CLT
instance Compare D4 D9 CLT
instance Pos (yi :* yl) => Compare D4 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D4 CGT
-- D5
instance Compare D5 D0 CGT
instance Compare D5 D1 CGT
instance Compare D5 D2 CGT
instance Compare D5 D3 CGT
instance Compare D5 D4 CGT
instance Compare D5 D5 CEQ
instance Compare D5 D6 CLT
instance Compare D5 D7 CLT
instance Compare D5 D8 CLT
instance Compare D5 D9 CLT
instance Pos (yi :* yl) => Compare D5 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D5 CGT
-- D6
instance Compare D6 D0 CGT
instance Compare D6 D1 CGT
instance Compare D6 D2 CGT
instance Compare D6 D3 CGT
instance Compare D6 D4 CGT
instance Compare D6 D5 CGT
instance Compare D6 D6 CEQ
instance Compare D6 D7 CLT
instance Compare D6 D8 CLT
instance Compare D6 D9 CLT
instance Pos (yi :* yl) => Compare D6 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D6 CGT
-- D7
instance Compare D7 D0 CGT
instance Compare D7 D1 CGT
instance Compare D7 D2 CGT
instance Compare D7 D3 CGT
instance Compare D7 D4 CGT
instance Compare D7 D5 CGT
instance Compare D7 D6 CGT
instance Compare D7 D7 CEQ
instance Compare D7 D8 CLT
instance Compare D7 D9 CLT
instance Pos (yi :* yl) => Compare D7 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D7 CGT
-- D8
instance Compare D8 D0 CGT
instance Compare D8 D1 CGT
instance Compare D8 D2 CGT
instance Compare D8 D3 CGT
instance Compare D8 D4 CGT
instance Compare D8 D5 CGT
instance Compare D8 D6 CGT
instance Compare D8 D7 CGT
instance Compare D8 D8 CEQ
instance Compare D8 D9 CLT
instance Pos (yi :* yl) => Compare D8 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D8 CGT
-- D9
instance Compare D9 D0 CGT
instance Compare D9 D1 CGT
instance Compare D9 D2 CGT
instance Compare D9 D3 CGT
instance Compare D9 D4 CGT
instance Compare D9 D5 CGT
instance Compare D9 D6 CGT
instance Compare D9 D7 CGT
instance Compare D9 D8 CGT
instance Compare D9 D9 CEQ
instance Pos (yi :* yl) => Compare D9 (yi :* yl) CLT
instance Pos (yi :* yl) => Compare (yi :* yl) D9 CGT


-- multidigit comparison
instance (Pos (xi :* xl), Pos (yi :* yl), Compare xl yl rl, Compare xi yi ri,
	  CS ri rl r) => Compare (xi :* xl) (yi :* yl) r

-- strengthen the comparison relation
class CS r1 r2 r3 | r1 r2 -> r3
instance CS CEQ r r
instance CS CGT r CGT
instance CS CLT r CLT

-- Abbreviated comparison assertions

-- | Equality abbreviated type-level assertion
class x :==: y
instance (Compare x y CEQ) => (:==:) x y -- ??? x :==: y fires an error 
                                         -- with ghc 6.8.2 


-- | Greater-than abbreviated type-level assertion
class x :>: y
instance (Compare x y CGT) => (:>:) x y 


-- | Lower-than abbreviated type-level assertion
class x :<: y
instance (Compare x y CLT) => (:<:) x y 


-- | Greater-than or equal abbreviated type-level assertion
class x :>=: y
instance (Succ x x', Compare x' y CGT) => (:>=:) x y 


-- | Lower-than or equal abbreviated type-level assertion
class x :<=: y
instance (Succ x' x, Compare x' y CLT) => (:<=:) x y



---------------------
-- Internal functions
---------------------


-- Spliting a decimal numeral into its initial digits and last digit
-- DivMod10 i l x 
-- it splits a decimal numeral x in two parts (i and l)
--  i = x `div` 10 (init, all digits but the last one)
--  l = x `mod` 10 (last, last digit)
-- Essentially, this is the general, non-structural, constructor/deconstructor

class (Nat i, Nat x) => DivMod10 i l x | i l -> x, x -> i l

instance DivMod10 D0 D0 D0
instance DivMod10 D0 D1 D1
instance DivMod10 D0 D2 D2
instance DivMod10 D0 D3 D3
instance DivMod10 D0 D4 D4
instance DivMod10 D0 D5 D5
instance DivMod10 D0 D6 D6
instance DivMod10 D0 D7 D7
instance DivMod10 D0 D8 D8
instance DivMod10 D0 D9 D9
instance (Nat (D1 :* l)) => DivMod10 D1 l  (D1 :* l)
instance (Nat (D2 :* l)) => DivMod10 D2 l  (D2 :* l)
instance (Nat (D3 :* l)) => DivMod10 D3 l  (D3 :* l)
instance (Nat (D4 :* l)) => DivMod10 D4 l  (D4 :* l)
instance (Nat (D5 :* l)) => DivMod10 D5 l  (D5 :* l)
instance (Nat (D6 :* l)) => DivMod10 D6 l  (D6 :* l)
instance (Nat (D7 :* l)) => DivMod10 D7 l  (D7 :* l)
instance (Nat (D8 :* l)) => DivMod10 D8 l  (D8 :* l)
instance (Nat (D9 :* l)) => DivMod10 D9 l  (D9 :* l)
instance (Nat (x :* l), Nat ((x :* l) :* l')) => 
  DivMod10 (x :* l) l' ((x :* l) :* l')

-- classify a natural as positive or zero
data Z
data P
class NClassify x r | x -> r
instance NClassify D0 Z
instance NClassify D1 P
instance NClassify D2 P
instance NClassify D3 P
instance NClassify D4 P
instance NClassify D5 P
instance NClassify D6 P
instance NClassify D7 P
instance NClassify D8 P
instance NClassify D9 P
instance Pos x => NClassify (x :* d) P
