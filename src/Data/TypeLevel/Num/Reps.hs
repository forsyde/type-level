{-# LANGUAGE EmptyDataDecls, TypeOperators, DeriveDataTypeable,
             ScopedTypeVariables #-}
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

import Data.Generics (Typeable)

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
-- | Decimal digit one
data D1 deriving Typeable
instance Show D1 where show _ = "1"
-- | Decimal digit two
data D2 deriving Typeable
instance Show D2 where show _ = "2"
-- | Decimal digit three 
data D3 deriving Typeable
instance Show D3 where show _ = "3"
-- | Decimal digit four 
data D4 deriving Typeable
instance Show D4 where show _ = "4"
-- | Decimal digit five
data D5 deriving Typeable
instance Show D5 where show _ = "5"
-- | Decimal digit six
data D6 deriving Typeable
instance Show D6 where show _ = "6"
-- | Decimal digit seven
data D7 deriving Typeable
instance Show D7 where show _ = "7"
-- | Decimal digit eight
data D8 deriving Typeable
instance Show D8 where show _ = "8"
-- | Decimal digit nine
data D9 deriving Typeable
instance Show D9 where show _ = "9"

-- | Connective to glue digits together.
--   For example, @D1 :* D0 :* D0@ represents the decimal number 100
data a :* b = a :* b deriving Typeable
instance (Show a, Show b) => Show (a :* b)
  where show _ = (show (undefined :: a)) ++ 
                 (show (undefined :: b))
