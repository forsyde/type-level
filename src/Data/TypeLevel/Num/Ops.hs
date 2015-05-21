{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators,
             FlexibleInstances, FlexibleContexts, UndecidableInstances,
             EmptyDataDecls, AllowAmbiguousTypes #-}
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
  Succ, succ,
  Pred, pred,
  -- * Addition/Subtraction
  Add, (+),
  Sub, (-),
  -- * Multiplication/Division
  Mul, (*),
  Div, div,
  Mod, mod,
  DivMod, divMod,
  IsDivBy, isDivBy,
  -- ** Special efficiency cases
  Mul10, mul10,
  Div10, div10,
  DivMod10, divMod10,
  -- * Exponientiation/Logarithm
  ExpBase, (^),
  LogBase, logBase,
  LogBaseF, logBaseF,
  IsPowOf, isPowOf,
  -- ** Special efficiency cases
  Exp10, exp10,
  Log10, log10,
  -- * Comparison assertions
  -- ** General comparison assertion
  Trich, trich,
  -- *** Type-level values denoting comparison results
  LT, EQ, GT,
  -- ** Abbreviated comparison assertions
  (:==:), (:>:), (:<:), (:>=:), (:<=:),
  (==)  , (>)  , (<)  , (>=)  , (<=), 
  -- * Maximum/Minimum
  Max, max,
  Min, min,
  -- * Greatest Common Divisor
  GCD, gcd
 ) where

import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Sets
import Data.TypeLevel.Bool

import Prelude hiding 
 (succ, pred, (+), (-), (*), div, mod, divMod, (^), logBase,
  (==), (>), (<), (<), (>=), (<=), max, min, gcd, Bool)

-------------------------
-- Successor, Predecessor
-------------------------

-- | Successor type-level relation. @Succ x y@ establishes
--  that @succ x = y@.
class (Nat x, Pos y) => Succ x y | x -> y, y -> x


instance (Pos y, IsZero y yz, DivMod10 x xi xl, Succ' xi xl yi yl yz,
         DivMod10 y yi yl)
   => Succ x y

class Succ' xh xl yh yl yz | xh xl -> yh yl yz, yh yl yz -> xh xl


-- This intends to implement a user reporting operation when
-- trying to calculate the predecesor of 0
-- FIXME: however, the instance rule is never triggered!

--class Failure t
---- No instances
--data PredecessorOfZeroError t
 
--instance Failure (PredecessorOfZeroError x) => Succ' (x,x) (x,x) D0 D0 True

instance Succ' xi D0 xi D1 False
instance Succ' xi D1 xi D2 False
instance Succ' xi D2 xi D3 False
instance Succ' xi D3 xi D4 False
instance Succ' xi D4 xi D5 False
instance Succ' xi D5 xi D6 False
instance Succ' xi D6 xi D7 False
instance Succ' xi D7 xi D8 False
instance Succ' xi D8 xi D9 False
instance Succ xi yi => Succ' xi D9 yi D0 False


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

-- | value-level reflection function for the 'Succ' type-level relation
succ :: Succ x y => x -> y
succ = undefined

-- Note: maybe redundant 
-- | Predecessor type-level relation. @Pred x y@ establishes
--  that @pred x = y@.
class (Pos x, Nat y) => Pred x y | x -> y, y -> x
instance Succ x y => Pred y x

-- | value-level reflection function for the 'Pred' type-level relation
pred :: Pred x y => x -> y
pred = undefined



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
-- TODO: explain
instance (Pos (xi :* xl), Nat z,
          Add' xi yi zi, DivMod10 y yi yl, Add' xl (zi :* yl) z)
    => Add' (xi :* xl) y z

-- | Addition type-level relation.  @Add x y z@ establishes
--  that @x + y = z@.
class (Add' x y z, Add' y x z) => Add x y z | x y -> z, z x -> y, z y -> x
instance (Add' x y z, Add' y x z) => Add x y z


-- | value-level reflection function for the 'Add' type-level relation 
(+) :: (Add x y z) => x -> y -> z
(+) = undefined

-- | Subtraction type-level relation. @Sub x y z@ establishes
--  that @x - y = z@ 
class Sub x y z | x y -> z, z x -> y, z y -> x
instance Add x y z => Sub z y x

-- | value-level reflection function for the 'Sub' type-level relation 
(-) :: (Sub x y z) => x -> y -> z
(-) = undefined
infixl 6 +, -

------------------------------
-- Multiplication and Division
------------------------------

-----------------
-- Multiplication
-----------------

-- | Multiplication type-level relation. @Mul x y z@ establishes
--  that @x * y = z@.
--   Note it isn't relational (i.e. its inverse cannot be used for division,
--   however, even if it could, the resulting division would only
--   work for zero-remainder divisions)
class (Nat x, Nat y, Nat z) => Mul x y z | x y -> z

-- By structural induction on the first argument
instance Nat y => Mul D0 y D0
instance Nat y => Mul D1 y y
instance Add y y z => Mul D2 y z
-- IMPORTANT: changing the line above by the commented line below
--            would make multiplication relational. However, that would
--            happen at the cost of performing a division by 2 in every 
--            multiplication which doesn't pay off.
--            Besides, the Division algortihm obtained out of the 
--            inverse of Mul can only work when the remainder is zero, 
--            which isn't really useful.
-- instance (Add y y z, DivMod z D2 y D0) => Mul D2 y z
instance (Add z y z', Mul D2 y z) => Mul D3 y z'
instance (Add z y z', Mul D3 y z) => Mul D4 y z'
instance (Add z y z', Mul D4 y z) => Mul D5 y z'
instance (Add z y z', Mul D5 y z) => Mul D6 y z'
instance (Add z y z', Mul D6 y z) => Mul D7 y z'
instance (Add z y z', Mul D7 y z) => Mul D8 y z'
instance (Add z y z', Mul D8 y z) => Mul D9 y z'
-- TODO explain.
instance (Pos (xi :* xl), Nat y, Mul xi y z, Mul10 z z10, Mul xl y dy, 
          Add dy z10 z')  => Mul (xi :* xl) y z'


-- | value-level reflection function for the multiplication type-level relation 
(*) :: Mul x y z => x -> y -> z
(*) = undefined
infixl 7 *

-----------
-- Division
-----------

-- | Division and Remainder type-level relation. @DivMod x y q r@ establishes
--  that @x/y = q + r/y@
--   Note it is not relational (i.e. its inverse cannot be used 
--   for multiplication). 
class (Nat x, Pos y) =>  DivMod x y q r | x y -> q r
instance (Pos y, Trich x y cmp, DivMod' x y q r cmp) => DivMod x y q r

-- FIXME: Is it required to introduce "q r x -> y" dependency as special case for q!=0?
class (Nat x, Pos y) => DivMod' x y q r cmp | x y cmp -> q r,
                                              q r y -> x

instance (Nat x, Pos y) => DivMod' x y D0 x  LT
instance (Pos x) => DivMod' x x D1 D0 EQ
instance (Nat x, Pos y, Sub x y x', Sub q D1 q', Trich x' y cmp, DivMod' x' y q' r cmp)
  => DivMod' x y q r GT

-- | value-level reflection function for the 'DivMod' type-level relation
divMod :: DivMod x y q r => x -> y -> (q,r)
divMod _ _ = (undefined, undefined)

-- | Division type-level relation. Remainder-discarding version of 'DivMod'. 
--   Note it is not relational (due to DivMod not being relational)
class Div x y z | x y -> z
instance (DivMod x y q r) => Div x y q

-- | value-level reflection function for the 'Div' type-level relation 
div :: Div x y z => x -> y -> z
div = undefined

-- | Remainder of division, type-level relation. @Mod x y r@ establishes that
--   @r@ is the reminder of dividing @x@ by @y@.
class Mod x y r | x y -> r
instance DivMod x y q r => Mod x y r

-- | value-level reflection function for the 'Mod' type-level relation 
mod :: Mod x y r => x -> y -> r
mod = undefined
infixl 7 `div`, `mod`


----------------------------------------
-- Multiplication/Division special cases
----------------------------------------

-- | Multiplication by 10 type-level relation (based on 'DivMod10').
--   @Mul10 x y@ establishes that @10 * x = y@.
class (Nat x, Nat q) => Mul10 x q | x -> q, q -> x
instance DivMod10 x q D0 => Mul10 q x

-- | value-level reflection function for 'Mul10' 
mul10 :: Mul10 x q => x -> q
mul10 = undefined

-- | Division by 10 and Remainer type-level relation (similar to 'DivMod'). 
--
--   This operation is much faster than DivMod. Furthermore, it is 
--   the general, non-structural, constructor/deconstructor since it
--   splits a decimal numeral into its initial digits and last digit.
--   Thus, it allows to inspect the structure of a number and is normally
--   used to create type-level operations.
--
--   Note that contrary to 'DivMod', 'DivMod10' is relational (it can be used to
--   multiply by 10)
class (Nat i, Nat x) => DivMod10 x i l | i l -> x, x -> i l
instance DivMod10 D0 D0 D0
instance DivMod10 D1 D0 D1
instance DivMod10 D2 D0 D2
instance DivMod10 D3 D0 D3
instance DivMod10 D4 D0 D4
instance DivMod10 D5 D0 D5
instance DivMod10 D6 D0 D6
instance DivMod10 D7 D0 D7
instance DivMod10 D8 D0 D8
instance DivMod10 D9 D0 D9
instance (Nat (D1 :* l)) => DivMod10 (D1 :* l) D1 l  
instance (Nat (D2 :* l)) => DivMod10 (D2 :* l) D2 l  
instance (Nat (D3 :* l)) => DivMod10 (D3 :* l) D3 l  
instance (Nat (D4 :* l)) => DivMod10 (D4 :* l) D4 l  
instance (Nat (D5 :* l)) => DivMod10 (D5 :* l) D5 l  
instance (Nat (D6 :* l)) => DivMod10 (D6 :* l) D6 l 
instance (Nat (D7 :* l)) => DivMod10 (D7 :* l) D7 l  
instance (Nat (D8 :* l)) => DivMod10 (D8 :* l) D8 l  
instance (Nat (D9 :* l)) => DivMod10 (D9 :* l) D9 l  
instance (Nat (x :* l), Nat ((x :* l) :* l')) => 
  DivMod10 ((x :* l) :* l') (x :* l) l' 

-- | value-level reflection function for DivMod10 
divMod10 :: DivMod10 x q r => x -> (q,r)
divMod10 _ = (undefined, undefined)


-- | Division by 10 type-level relation (based on DivMod10)
class (Nat x, Nat q) => Div10 x q | x -> q
instance DivMod10 x q r => Div10 x q

-- | value-level reflection function for Mul10 
div10 :: Div10 x q => x -> q
div10 = undefined

----------------------------
-- Is-Divisible-By assertion
----------------------------

-- | Is-divisible-by type-level assertion. e.g @IsDivBy d x@ establishes that
--   @x@ is divisible by @d@.
class (Pos d, Nat x) => IsDivBy d x
instance (DivMod x d q D0) => IsDivBy d x

-- | value-level reflection function for IsDivBy
isDivBy :: IsDivBy d x => d -> x
isDivBy = undefined

---------------------------
-- Exponentiation/Logarithm
---------------------------

-- | Exponentation type-level relation. @ExpBase b e r@ establishes
--  that @b^e = r@
--  Note it is not relational (i.e. it cannot be used to express logarithms)
class (Nat b, Nat e, Nat r) => ExpBase b e r | b e -> r

-- structural induction over the exponent
instance Nat b => ExpBase b D0 D1
instance Nat b => ExpBase b D1 b
instance (Mul b b r) => ExpBase b D2 r
instance (Mul r b r', ExpBase b D2 r) => ExpBase b D3 r'
instance (Mul r b r', ExpBase b D3 r) => ExpBase b D4 r'
instance (Mul r b r', ExpBase b D4 r) => ExpBase b D5 r'
instance (Mul r b r', ExpBase b D5 r) => ExpBase b D6 r'
instance (Mul r b r', ExpBase b D6 r) => ExpBase b D7 r'
instance (Mul r b r', ExpBase b D7 r) => ExpBase b D8 r'
instance (Mul r b r', ExpBase b D8 r) => ExpBase b D9 r'
instance (Nat b, Pos (ei :* el), Nat r, 
          Mul b r r', Pred (ei :* el) e', ExpBase b e' r) 
           => ExpBase b (ei :* el) r'
-- | value-level reflection function for the ExpBase type-level relation
(^) :: ExpBase b e r => b -> e -> r
(^) = undefined
infixr 8 ^

-- Logarithm type-level relation. @LogBase b x e@ establishes that 
-- @log_base_b x = e@
--  Note it is not relational (i.e. cannot be used to express exponentiation)
class (Pos b, b :>=: D2, Pos x, Nat e) =>  LogBase b x e  | b x -> e 
instance  LogBaseF b x e f => LogBase b x e


-- | value-level reflection function for LogBase
logBase :: LogBaseF b x e f => b -> x -> e
logBase = undefined 


-- | Version of LogBase which also outputs if the logarithm
-- calculated was exact.
-- f indicates if the resulting logarithm has no fractional part (i.e.
-- tells if the result provided is exact)
class (Pos b, b :>=: D2, Pos x, Nat e, Bool f) 
     =>  LogBaseF b x e f | b x -> e f
instance (Trich x b cmp, LogBaseF' b x e f cmp) => LogBaseF b x e f

class (Pos b, b :>=: D2, Pos x, Nat e, Bool f)
     => LogBaseF' b x e f cmp | b x cmp -> e f 
instance (Pos b, b :>=: D2, Pos x) => LogBaseF' b x D0 False LT
instance (Pos b, b :>=: D2) => LogBaseF' b b D1 True  EQ
instance (Pos b, b :>=: D2, Pos x, DivMod x b q r, IsZero r rz, And rz f' f, 
          Pred e e', LogBaseF b q e' f') => LogBaseF' b x e f GT

-- | value-level reflection function for LogBaseF
logBaseF :: LogBaseF b x e f => b -> x -> (e,f)
logBaseF _ _ = (undefined, undefined) 


-- We could reuse LogBaseF for IsPowOf but it would be inneficient.
-- LogBaseF continues calculating the logarithm even if after knowing its
-- not exact. Thus, it is desirable to include a custom definition of
-- IsPowOf which can "abort" the calculation forcing the Divisions to be
-- exact


-- | Assert that a number (@x@) can be expressed as the power of another one
--   (@b@) (i.e. the fractional part of @log_base_b x = 0@, or, 
--   in a different way, @exists y . b\^y = x@). 
class (Pos b, b :>=: D2, Pos x) =>  IsPowOf b x
instance (Trich x b cmp, IsPowOf' b x cmp) => IsPowOf b x

class (Pos b, b :>=: D2, Pos x) => IsPowOf' b x cmp
-- If lower (x < b), then the logarithm is not exact  
-- instance (Pos b, b :>=: D2, Pos x) => IsPowOf' b x LT
instance (Pos b, b :>=: D2) => IsPowOf' b b EQ
instance (Pos b, b :>=: D2, Pos x, DivMod x b q D0, IsPowOf b q) 
         => IsPowOf' b x  GT
-- | 
isPowOf :: IsPowOf b x => b -> x -> ()
isPowOf = undefined

-----------------------------------
-- Base-10 Exponentiation/Logarithm
-----------------------------------

-- | Base-10 Exponentiation type-level relation
class (Nat x, Pos y) => Exp10 x y | x -> y, y -> x
instance Exp10 D0 D1
instance Exp10 D1 (D1 :* D0)
instance Exp10 D2 (D1 :* D0 :* D0)
instance Exp10 D3 (D1 :* D0 :* D0 :* D0)
instance Exp10 D4 (D1 :* D0 :* D0 :* D0 :* D0)
instance Exp10 D5 (D1 :* D0 :* D0 :* D0 :* D0 :* D0)
instance Exp10 D6 (D1 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)
instance Exp10 D7 (D1 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)
instance Exp10 D8 (D1 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)
instance Exp10 D9 (D1 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)
instance (Pred (xi :* xl) x', 
          Exp10 x' (y :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0))
      => Exp10 (xi :* xl) 
               (y :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)

-- | value-level reflection function for Exp10
exp10 :: Exp10 x y => x -> y
exp10 = undefined

-- | Base-10 logarithm type-level relation
--   Note it is not relational (cannot be used to express Exponentation to 10)
--   However, it works with any positive numeral (not just powers of 10)
class (Pos x, Nat y) => Log10 x y | x -> y
instance Log10 D1 D0
instance Log10 D2 D0
instance Log10 D3 D0
instance Log10 D4 D0
instance Log10 D5 D0
instance Log10 D6 D0
instance Log10 D7 D0
instance Log10 D8 D0
instance Log10 D9 D0
instance (Pos (xi :* xl), Pred y y', Log10 xi y') => Log10 (xi :* xl) y

-- | value-level reflection function for 'Log10'
log10 :: Log10 x y => x -> y
log10 = undefined

{- Log10': Alternative implementation of Log10

Relational, but it only works for results of Exp10 (i.e. powers of 10).

class (Pos x, Nat y) => Log10' x y | x -> y, y -> x
instance Exp10 x y => Log10' y x
-}


-------------
-- Comparison
-------------

-- type-level values denoting comparison results

-- | Lower than 
data LT
-- | Equal
data EQ
-- | Greater than
data GT

-- | Trichotomy type-level relation. 'Trich x y r' establishes
--   the relation (@r@) between @x@ and @y@. The obtained relation (@r@)
--   Can be 'LT' (if @x@ is lower than @y@), 'EQ' (if @x@ equals @y@) or
--   'GT' (if @x@ is greater than @y@)
class (Nat x, Nat y) => Trich x y r | x y -> r

-- | value-level reflection function for the comparison type-level assertion 
trich :: Trich x y r => z -> x -> r
trich = undefined

-- by structural induction on the first, and then the second argument
-- D0
instance Trich D0 D0 EQ
instance Trich D0 D1 LT
instance Trich D0 D2 LT
instance Trich D0 D3 LT
instance Trich D0 D4 LT
instance Trich D0 D5 LT
instance Trich D0 D6 LT
instance Trich D0 D7 LT
instance Trich D0 D8 LT
instance Trich D0 D9 LT
instance Pos (yi :* yl) => Trich D0 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D0 GT
-- D1
instance Trich D1 D0 GT
instance Trich D1 D1 EQ
instance Trich D1 D2 LT
instance Trich D1 D3 LT 
instance Trich D1 D4 LT
instance Trich D1 D5 LT 
instance Trich D1 D6 LT
instance Trich D1 D7 LT 
instance Trich D1 D8 LT
instance Trich D1 D9 LT
instance Pos (yi :* yl) => Trich D1 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D1 GT
-- D2
instance Trich D2 D0 GT
instance Trich D2 D1 GT
instance Trich D2 D2 EQ
instance Trich D2 D3 LT
instance Trich D2 D4 LT
instance Trich D2 D5 LT
instance Trich D2 D6 LT
instance Trich D2 D7 LT
instance Trich D2 D8 LT
instance Trich D2 D9 LT
instance Pos (yi :* yl) => Trich D2 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D2 GT
-- D3
instance Trich D3 D0 GT
instance Trich D3 D1 GT
instance Trich D3 D2 GT
instance Trich D3 D3 EQ
instance Trich D3 D4 LT
instance Trich D3 D5 LT
instance Trich D3 D6 LT
instance Trich D3 D7 LT
instance Trich D3 D8 LT
instance Trich D3 D9 LT
instance Pos (yi :* yl) => Trich D3 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D3 GT
-- D4
instance Trich D4 D0 GT
instance Trich D4 D1 GT
instance Trich D4 D2 GT
instance Trich D4 D3 GT
instance Trich D4 D4 EQ
instance Trich D4 D5 LT
instance Trich D4 D6 LT
instance Trich D4 D7 LT
instance Trich D4 D8 LT
instance Trich D4 D9 LT
instance Pos (yi :* yl) => Trich D4 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D4 GT
-- D5
instance Trich D5 D0 GT
instance Trich D5 D1 GT
instance Trich D5 D2 GT
instance Trich D5 D3 GT
instance Trich D5 D4 GT
instance Trich D5 D5 EQ
instance Trich D5 D6 LT
instance Trich D5 D7 LT
instance Trich D5 D8 LT
instance Trich D5 D9 LT
instance Pos (yi :* yl) => Trich D5 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D5 GT
-- D6
instance Trich D6 D0 GT
instance Trich D6 D1 GT
instance Trich D6 D2 GT
instance Trich D6 D3 GT
instance Trich D6 D4 GT
instance Trich D6 D5 GT
instance Trich D6 D6 EQ
instance Trich D6 D7 LT
instance Trich D6 D8 LT
instance Trich D6 D9 LT
instance Pos (yi :* yl) => Trich D6 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D6 GT
-- D7
instance Trich D7 D0 GT
instance Trich D7 D1 GT
instance Trich D7 D2 GT
instance Trich D7 D3 GT
instance Trich D7 D4 GT
instance Trich D7 D5 GT
instance Trich D7 D6 GT
instance Trich D7 D7 EQ
instance Trich D7 D8 LT
instance Trich D7 D9 LT
instance Pos (yi :* yl) => Trich D7 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D7 GT
-- D8
instance Trich D8 D0 GT
instance Trich D8 D1 GT
instance Trich D8 D2 GT
instance Trich D8 D3 GT
instance Trich D8 D4 GT
instance Trich D8 D5 GT
instance Trich D8 D6 GT
instance Trich D8 D7 GT
instance Trich D8 D8 EQ
instance Trich D8 D9 LT
instance Pos (yi :* yl) => Trich D8 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D8 GT
-- D9
instance Trich D9 D0 GT
instance Trich D9 D1 GT
instance Trich D9 D2 GT
instance Trich D9 D3 GT
instance Trich D9 D4 GT
instance Trich D9 D5 GT
instance Trich D9 D6 GT
instance Trich D9 D7 GT
instance Trich D9 D8 GT
instance Trich D9 D9 EQ
instance Pos (yi :* yl) => Trich D9 (yi :* yl) LT
instance Pos (yi :* yl) => Trich (yi :* yl) D9 GT


-- multidigit comparison
instance (Pos (xi :* xl), Pos (yi :* yl), Trich xl yl rl, Trich xi yi ri,
	  CS ri rl r) => Trich (xi :* xl) (yi :* yl) r

-- strengthen the comparison relation
class CS r1 r2 r3 | r1 r2 -> r3
instance CS EQ r r
instance CS GT r GT
instance CS LT r LT

-- Abbreviated comparison assertions

-- | Equality abbreviated type-level assertion
class x :==: y
instance (Trich x y EQ) => (:==:) x y -- ??? x :==: y fires an error 
                                         -- with ghc 6.8.2 

-- | value-level reflection function for the equality abbreviated 
--   type-level assertion 
(==) :: (x :==: y) => x -> y -> ()
(==) = undefined

-- | Greater-than abbreviated type-level assertion
class x :>: y
instance (Trich x y GT) => (:>:) x y 

-- | value-level reflection function for the equality abbreviated 
--   type-level assertion 
(>) :: (x :>: y) => x -> y -> ()
(>) = undefined

-- | Lower-than abbreviated type-level assertion
class x :<: y
instance (Trich x y LT) => (:<:) x y 

-- | value-level reflection function for the lower-than abbreviated 
--   type-level assertion 
(<) :: (x :<: y) => x -> y -> ()
(<) = undefined

-- | Greater-than or equal abbreviated type-level assertion
class x :>=: y
instance (Succ x x', Trich x' y GT) => (:>=:) x y 

-- | value-level reflection function for the greater-than or equal abbreviated 
--   type-level assertion 
(>=) :: (x :>=: y) => x -> y -> ()
(>=) = undefined

-- | Lower-than or equal abbreviated type-level assertion
class x :<=: y
instance (Succ x' x, Trich x' y LT) => (:<=:) x y

-- | value-level reflection function for the lower-than or equal abbreviated 
--   type-level assertion 
(<=) :: (x :<=: y) => x -> y -> ()
(<=) = undefined
infix 4 <,<=,>=,>,==
------------------
-- Maximum/Minimum
------------------

-- Choose the largest of x and y in the order b
class Max' x y b r | x y b -> r
instance Max' x y LT y
instance Max' x y EQ y
instance Max' x y GT x

-- | Maximum type-level relation
class Max x y z | x y -> z
instance (Max' x y b z, Trich x y b) => Max x y z

-- | value-level reflection function for the maximum type-level relation
max :: Max x y z => x -> y -> z
max = undefined

-- | Minimum type-level relation
class Min x y z | x y -> z
instance (Max' y x b z, Trich x y b) => Min x y z


-- | value-level reflection function for the minimum type-level relation
min :: Min x y z => x -> y -> z
min = undefined

-------
-- GCD
-------

-- | Greatest Common Divisor type-level relation
class (Nat x, Nat y, Nat gcd) => GCD x y gcd | x y -> gcd
instance (Nat x, Nat y, Trich x y cmp, IsZero y yz, GCD' x y yz cmp gcd)
   => GCD x y gcd

-- Euclidean algorithm 
class (Nat x, Nat y, Nat gcd) => GCD' x y yz cmp gcd | x y yz cmp -> gcd
instance Nat x => GCD' x D0 True cmp D0
instance (Nat x, Nat y, GCD y x gcd) => GCD' x y False LT gcd
instance Nat x => GCD' x x  False EQ x
instance (Nat x, Nat y, Sub x y x', GCD x' y gcd) => GCD' x y False GT gcd

-- | value-level reflection function for the GCD type-level relation
gcd :: GCD x y z => x -> y -> z
gcd = undefined

---------------------
-- Internal functions
---------------------

-- classify a natural as positive or zero
class IsZero x r | x -> r
instance IsZero D0 True
instance IsZero D1 False
instance IsZero D2 False
instance IsZero D3 False
instance IsZero D4 False
instance IsZero D5 False
instance IsZero D6 False
instance IsZero D7 False
instance IsZero D8 False
instance IsZero D9 False
instance Pos x => IsZero (x :* d) False
