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
-- Type-level numerical sets. Currently there is only support for Naturals and 
-- Positives.
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Num.Sets (Pos, Nat, toNum, toInt) where 

import Data.TypeLevel.Num.Reps

-----------
-- Naturals
-----------


-- The well-formedness condition, the kind predicate.
-- These classes are internal, denoted by the ending "I", which is removed in 
-- the exported proxies (read below)

-- | Naturals (Positives without zero), internal version
class NatI n where 
-- | Reflecting function
 toNum :: Num a => n -> a -- Reflection function

-- | Less generic reflecting function (Int)
toInt :: Nat n => n -> Int
toInt =  toNum



-- | Positives (Naturals without zero), internal version
class NatI n => PosI n

-- To prevent the user from adding new instances to NatI and especially
-- to PosI (e.g., to prevent the user from adding the instance |Pos D0|)
-- we do NOT export NatI and PosI. Rather, we export the following proxies.
-- The proxies entail PosI and NatI and so can be used to add PosI and NatI
-- constraints in the signatures. However, all the constraints below
-- are expressed in terms of NatI and PosI rather than proxies. Thus,
-- even if the user adds new instances to proxies, it would not matter.
-- Besides, because the following proxy instances are most general,
-- one may not add further instances without overlapping instance extension.

-- | Naturals (Positives without zero)
class    NatI n => Nat n
instance NatI n => Nat n

-- | Positives (Naturals without zero)
class    PosI n => Pos n
instance PosI n => Pos n

--------------------
-- Natural Instances
--------------------

-- Note: TH would be helpful to sistematically define instances 
--       (our type level operations)
--       However, type-splicing is not yet implemented in GHC :S

-- monodigit naturals
instance NatI D0 where toNum _ = fromInteger 0
instance NatI D1 where toNum _ = fromInteger 1
instance NatI D2 where toNum _ = fromInteger 2
instance NatI D3 where toNum _ = fromInteger 3
instance NatI D4 where toNum _ = fromInteger 4
instance NatI D5 where toNum _ = fromInteger 5
instance NatI D6 where toNum _ = fromInteger 6
instance NatI D7 where toNum _ = fromInteger 7
instance NatI D8 where toNum _ = fromInteger 8
instance NatI D9 where toNum _ = fromInteger 9

-- multidigit naturals
-- Note: The PosI constraint guarantees that all valid representations are 
-- normalized (i.e. D0 :* D1 will lead to a compiler error)
-- Note as well that ill-formed representations such as
-- (D1 :* D2) :* (D3 :* D4) are not recognized as instances of
-- naturals nor positives.
instance PosI x => NatI (x :* D0) where toNum n = subLastDec n
instance PosI x => NatI (x :* D1) where toNum n = subLastDec n + fromInteger 1
instance PosI x => NatI (x :* D2) where toNum n = subLastDec n + fromInteger 2
instance PosI x => NatI (x :* D3) where toNum n = subLastDec n + fromInteger 3
instance PosI x => NatI (x :* D4) where toNum n = subLastDec n + fromInteger 4
instance PosI x => NatI (x :* D5) where toNum n = subLastDec n + fromInteger 5
instance PosI x => NatI (x :* D6) where toNum n = subLastDec n + fromInteger 6
instance PosI x => NatI (x :* D7) where toNum n = subLastDec n + fromInteger 7
instance PosI x => NatI (x :* D8) where toNum n = subLastDec n + fromInteger 8
instance PosI x => NatI (x :* D9) where toNum n = subLastDec n + fromInteger 9

-- monodigit positives
instance PosI D1
instance PosI D2
instance PosI D3
instance PosI D4
instance PosI D5
instance PosI D6
instance PosI D7
instance PosI D8
instance PosI D9

-- multidigit positives
-- Note: The PosI constraint guarantees that all valid representations are 
-- normalized (i.e. D0 :* D1 will lead to a compiler error)
instance PosI x => PosI (x :* D0)
instance PosI x => PosI (x :* D1)
instance PosI x => PosI (x :* D2)
instance PosI x => PosI (x :* D3)
instance PosI x => PosI (x :* D4)
instance PosI x => PosI (x :* D5)
instance PosI x => PosI (x :* D6)
instance PosI x => PosI (x :* D7)
instance PosI x => PosI (x :* D8)
instance PosI x => PosI (x :* D9)



---------------------
-- Internal functions
---------------------

-- substract the last digit of a decimal type-level numeral and obtain 
-- the result's reflected value 
{-# INLINE subLastDec #-}
subLastDec :: (Num a, NatI (x :* d), NatI x) => x :* d -> a
subLastDec = (10*).toNum.div10Dec

-- Divide a decimal type-level numeral by 10 
{-# INLINE div10Dec #-} 
div10Dec :: NatI (x :* d) => x :* d -> x
div10Dec _ = undefined
