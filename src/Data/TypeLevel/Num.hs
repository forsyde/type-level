-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel.Num
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module is a wrapper for all the publicly usable numerical types and 
-- functions of the type-level library.
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Num 
 (module Data.TypeLevel.Num.Reps,
  module Data.TypeLevel.Num.Aliases,
  module Data.TypeLevel.Num.Sets,
  module Data.TypeLevel.Num.Ops) where

import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases
import Data.TypeLevel.Num.Sets
import Data.TypeLevel.Num.Ops
