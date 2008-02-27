{-# LANGUAGE EmptyDataDecls, TypeOperators, DeriveDataTypeable,
             ScopedTypeVariables, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel.Num.Reps
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (TypeOperators)
--
-- Type-level numerical representations. Currently, only decimals are 
-- supported.
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Num.Reps (
 -- * Decimal representation
 --   $decdescription
 --  ** Digits
 D0, D1, D2, D3, D4, D5, D6, D7, D8, D9,
 --  ** Connective
 (:*)(..),
 ) where

import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Lift(..)) 

-------------------------
-- Decimal Representation
-------------------------

-- $decdescription 
-- Decimals are represented using a different type (@Dx@) for each digit and a 
-- binary infix connective (@:*@) to enable forming arbitrary precision 
-- multidigit numbers. For example @D0@ represents number 0, @D4 :* D2@ 
-- represents number 42, @D1 :* D0 :* D0@ represents 100, etc ... Obviously, 
-- negative numbers cannot be represented.

-- | Decimal digit zero
data D0 deriving Typeable
instance Show D0 where show _ = "0"
instance Lift D0 where lift _ = [| undefined :: D0 |]
-- | Decimal digit one
data D1 deriving Typeable
instance Show D1 where show _ = "1"
instance Lift D1 where lift _ = [| undefined :: D1 |]
-- | Decimal digit two
data D2 deriving Typeable
instance Show D2 where show _ = "2"
instance Lift D2 where lift _ = [| undefined :: D2 |]
-- | Decimal digit three 
data D3 deriving Typeable
instance Show D3 where show _ = "3"
instance Lift D3 where lift _ = [| undefined :: D3 |]
-- | Decimal digit four 
data D4 deriving Typeable
instance Show D4 where show _ = "4"
instance Lift D4 where lift _ = [| undefined :: D4 |]
-- | Decimal digit five
data D5 deriving Typeable
instance Show D5 where show _ = "5"
instance Lift D5 where lift _ = [| undefined :: D5 |]
-- | Decimal digit six
data D6 deriving Typeable
instance Lift D6 where lift _ = [| undefined :: D6 |]
instance Show D6 where show _ = "6"
-- | Decimal digit seven
data D7 deriving Typeable
instance Show D7 where show _ = "7"
instance Lift D7 where lift _ = [| undefined :: D7 |]
-- | Decimal digit eight
data D8 deriving Typeable
instance Show D8 where show _ = "8"
instance Lift D8 where lift _ = [| undefined :: D8 |]
-- | Decimal digit nine
data D9 deriving Typeable
instance Show D9 where show _ = "9"
instance Lift D9 where lift _ = [| undefined :: D9 |]

-- | Connective to glue digits together.
--   For example, @D1 :* D0 :* D0@ represents the decimal number 100
data a :* b = a :* b deriving Typeable

instance (Show a, Show b) => Show (a :* b) where
  show _ = (show (undefined :: a)) ++ (show (undefined :: b))

instance (Lift a, Lift b) => Lift (a :* b) where
  lift _ = [| $(lift (undefined ::a)) :* $(lift (undefined :: b) ) |]