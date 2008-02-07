{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel.Num.Sets
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (non-standard instances)
--
-- Type-level numerical sets. Currently there is only support for Naturals.
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Num.Sets (Nat, Nat0, toInt) where 

import Data.TypeLevel.Num.Reps

-----------
-- Naturals
-----------


-- The well-formedness condition, the kind predicate.
-- These classes are internal, denoted by the ending "I", which is removed in 
-- the exported proxies (read below)

-- | Naturals
class Nat0I n where 
 toInt :: n -> Int -- Reflection function

-- | Positives (Naturals without zero)
class Nat0I n => NatI n

-- To prevent the user from adding new instances to Nat0I and especially
-- to NatI (e.g., to prevent the user from adding the instance |Nat D0|)
-- we do NOT export Nat0I and NatI. Rather, we export the following proxies.
-- The proxies entail NatI and Nat0I and so can be used to add NatI and Nat0I
-- constraints in the signatures. However, all the constraints below
-- are expressed in terms of Nat0I and NatI rather than proxies. Thus,
-- even if the user adds new instances to proxies, it would not matter.
-- Besides, because the following proxy instances are most general,
-- one may not add further instances without overlapping instance extension.
class    Nat0I n => Nat0 n
instance Nat0I n => Nat0 n
class    NatI n => Nat n
instance NatI n => Nat n

--------------------
-- Natural Instances
--------------------

-- Note: TH would be helpful to sistematically define instances 
--       (our type level operations)
--       However, type-splicing is not yet implemented in GHC :S

-- monodigit naturals
instance Nat0I D0 where toInt _ = 0
instance Nat0I D1 where toInt _ = 1
instance Nat0I D2 where toInt _ = 2
instance Nat0I D3 where toInt _ = 3
instance Nat0I D4 where toInt _ = 4
instance Nat0I D5 where toInt _ = 5
instance Nat0I D6 where toInt _ = 6
instance Nat0I D7 where toInt _ = 7
instance Nat0I D8 where toInt _ = 8
instance Nat0I D9 where toInt _ = 9

-- multidigit naturals
-- Note: The NatI constraint guarantees that all valid representations are 
-- normalized (i.e. D0 :+ D1 will lead to a compiler error)
-- Note as well that ill-formed representations such as
-- (D1 :+ D2) :+ (D3 :+ D4) are not recognized as instances of
-- Naturals nor positives.
instance NatI x => Nat0I (x :+ D0) where toInt n = subLastDec n
instance NatI x => Nat0I (x :+ D1) where toInt n = subLastDec n + 1
instance NatI x => Nat0I (x :+ D2) where toInt n = subLastDec n + 2
instance NatI x => Nat0I (x :+ D3) where toInt n = subLastDec n + 3
instance NatI x => Nat0I (x :+ D4) where toInt n = subLastDec n + 4
instance NatI x => Nat0I (x :+ D5) where toInt n = subLastDec n + 5
instance NatI x => Nat0I (x :+ D6) where toInt n = subLastDec n + 6
instance NatI x => Nat0I (x :+ D7) where toInt n = subLastDec n + 7
instance NatI x => Nat0I (x :+ D8) where toInt n = subLastDec n + 8
instance NatI x => Nat0I (x :+ D9) where toInt n = subLastDec n + 9

-- monodigit positives
instance NatI D1
instance NatI D2
instance NatI D3
instance NatI D4
instance NatI D5
instance NatI D6
instance NatI D7
instance NatI D8
instance NatI D9

-- multidigit positives
-- Note: The NatI constraint guarantees that all valid representations are 
-- normalized (i.e. D0 :+ D1 will lead to a compiler error)
instance NatI x => NatI (x :+ D0)
instance NatI x => NatI (x :+ D1)
instance NatI x => NatI (x :+ D2)
instance NatI x => NatI (x :+ D3)
instance NatI x => NatI (x :+ D4)
instance NatI x => NatI (x :+ D5)
instance NatI x => NatI (x :+ D6)
instance NatI x => NatI (x :+ D7)
instance NatI x => NatI (x :+ D8)
instance NatI x => NatI (x :+ D9)



---------------------
-- Internal functions
---------------------

-- substract the last digit of a decimal type-level numeral and obtain 
-- the result's reflected value 
{-# INLINE subLastDec #-}
subLastDec :: (Nat0I x, Nat0I (x :+ d)) => x :+ d -> Int 
subLastDec = (10*).toInt.div10Dec

-- Divide a decimal type-level numeral by 10 
{-# INLINE div10Dec #-} 
div10Dec :: Nat0I (x :+ d) => x :+ d -> x
div10Dec _ = undefined
