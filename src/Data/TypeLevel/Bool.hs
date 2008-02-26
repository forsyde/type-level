{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies,
             Rank2Types, DeriveDataTypeable, FlexibleInstances,
             UndecidableInstances, FlexibleContexts  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel.Bool
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental (MPTC, non-standarad instances)
-- Portability :  non-portable
--
-- Type-level Booleans.
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Bool (
    -- * Type-level boolean values
    Bool,
    False, false,
    True, true,
    reifyBool,
    -- * Type-level boolean operations
    Not, not,
    And, (&&),
    Or, (||),
    Xor, xor,
    Imp, imp,
    Eq, eq

) where

import Data.Generics (Typeable)
import Prelude hiding (Bool, not, (&&), (||), Eq)
import qualified Prelude as P

------------------------------------
-- Definition of type-level Booleans
------------------------------------

-- | True type-level value
data True deriving Typeable

instance Show True where
 show _ = "True"

-- | True value-level reflecting function
true :: True
true = undefined

-- | False type-level value
data False deriving Typeable

instance Show False where
 show _ = "False"


-- | False value-level reflecting function
false :: False
false = undefined


-- | Booleans, internal version
class BoolI b where
 toBool :: b -> P.Bool


-- To prevent the user from adding new instances to BoolI we do NOT export 
-- BoolI itself. Rather, we export the following proxy (Bool). 
-- The proxy entails BoolI and so can be used to add BoolI 
-- constraints in the signatures. However, all the constraints below
-- are expressed in terms of BoolI rather than the proxy. Thus, even if the 
-- user adds new instances to the proxy, it would not matter. 
-- Besides, because the following proxy instances are most general,
-- one may not add further instances without the overlapping instances 
-- extension.

-- | Type-level Booleans
class BoolI b => Bool b

instance BoolI b => Bool b

instance BoolI True where
 toBool _ = True

instance BoolI False where
 toBool _ = False

-- | Reification function. In CPS style (best possible solution)
reifyBool :: P.Bool -> (forall b . Bool b => b -> r) -> r
reifyBool True  f = f true
reifyBool False f = f false

-------------
-- Operations
-------------

-- Not type-level relation
class (BoolI b1, BoolI b2) => Not b1 b2 | b1 -> b2, b2 -> b1
instance Not False True
instance Not True  False

-- | value-level reflection function for the Not type-level relation
not :: Not b1 b2 => b1 -> b2
not = undefined

-- And type-level relation
class (BoolI b1, BoolI b2, BoolI b3) => And b1 b2 b3 | b1 b2 -> b3
instance And False False False  
instance And False True  False  
instance And True  False False  
instance And True  True  True


-- | value-level reflection function for the And type-level relation
(&&) :: And b1 b2 b3 => b1 -> b2 -> b3
(&&) = undefined

  
-- Or type-level relation
class (BoolI b1, BoolI b2, BoolI b3) => Or b1 b2 b3 | b1 b2 -> b3
instance Or False False False  
instance Or False True  True  
instance Or True  False True  
instance Or True  True  True


-- | value-level reflection function for the Or type-level relation
(||) :: Or b1 b2 b3 => b1 -> b2 -> b3
(||) = undefined

-- Exclusive or type-level relation
class (BoolI b1, BoolI b2, BoolI b3) => Xor b1 b2 b3 | b1 b2 -> b3
instance Xor False False False  
instance Xor False True  True  
instance Xor True  False True  
instance Xor True  True  False

-- | value-level reflection function for the Xor type-level relation
xor :: Xor b1 b2 b3 => b1 -> b2 -> b3
xor = undefined


-- Implication type-level relation
class (BoolI b1, BoolI b2, BoolI b3) => Imp b1 b2 b3 | b1 b2 -> b3
instance Imp False False True  
instance Imp False True  True  
instance Imp True  False False  
instance Imp True  True  True


-- | value-level reflection function for the Imp type-level relation
imp :: Imp b1 b2 b3 => b1 -> b2 -> b3
imp = undefined


-- Although equality can be defined as the composition of Xor and Not
-- we define it specifically

-- | Boolean equality type-level relation
class (BoolI b1, BoolI b2, BoolI b3) => Eq b1 b2 b3 | b1 b2 -> b3
instance Eq False False True  
instance Eq False True  False  
instance Eq True  False False  
instance Eq True  True  True


-- FIXME: eq should be named (==) but it clashes with the (==) defined
--        in Data.TypeLevel.Num . The chosen ugly solution was to rename it to
--        eq.

-- | value-level reflection function for the Eq type-level relation
eq :: Eq b1 b2 b3 => b1 -> b2 -> b3
eq = undefined

