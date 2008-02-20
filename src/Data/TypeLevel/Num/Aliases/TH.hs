{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel.Num.Aliases
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- Internal template haskell functions to generate type-level numeral aliases
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Num.Aliases.TH (genAliases, dec2TypeLevel) where

import Language.Haskell.TH

import Data.TypeLevel.Num.Reps

data Base = Bin | Oct | Dec | Hex

base2Int :: Base -> Int
base2Int Bin = 2
base2Int Oct = 8
base2Int Dec = 10
base2Int Hex = 16

-- This module needs to be separated from Data.TypeLevel.Num.Aliases due to
-- a limitation in Template Haskell implementation: 
-- "You can only run a function at compile time if it is imported from another 
-- module." 

genAliases :: Int -- how many binary aliases 
           -> Int -- how many octal aliases
           -> Int -- how many dec aliases
           -> Int -- how many hex aliases
           -> Q [Dec]
genAliases nb no nd nh = genAliases' nb no nd nh (maximum [nb,no,nd,nh])

genAliases' :: Int -- how many binary aliases 
            -> Int -- how many octal aliases
            -> Int -- how many dec aliases
            -> Int -- how many hex aliases
            -> Int -- maximum alias
            -> Q [Dec]
-- FIXME: genAliases' is ugly!
genAliases' nb no nd nh curr 
 | curr < 0 = return []
 | otherwise = 
    do rest <- genAliases' nb no nd nh (curr-1)
       -- binaries
       restb <- addAliasBase (curr > nb) ('b' : bStr) ('B' : bStr) rest
       -- octals
       resto <- addAliasBase (curr > no) ('o' : oStr) ('O' : oStr) restb
       -- decimals, we don't aliases of the decimal digits
       -- (they are alredy defined in the representation module)
       restd <- if curr > nd then return resto             
                else do val <- genValAlias ('d' : dStr) decRep
                        typ <- genTypeAlias ('D' : dStr) decRep
                        if (curr < 10) then return $ val : resto
                         else return $ val : typ : resto
       -- hexadicimals
       addAliasBase (curr > no) ('h' : hStr) ('H' : hStr) restd
 
 where  -- Add aliases of certain base to the rest of aliases
        addAliasBase cond vStr tStr rest =
          if cond then return rest
          else  do val <- genValAlias vStr decRep
                   typ <- genTypeAlias tStr decRep
                   return $ val : typ : rest

        decRep = dec2TypeLevel curr        

        bStr = toBase Bin curr
        oStr = toBase Oct curr
        dStr = toBase Dec curr
        hStr = toBase Hex curr

-- | Generate the type-level decimal representation for a value-level 
--   natural number. 
-- NOTE: This function could be useful by itself avoiding to generate 
-- aliases. However, type-splicing is not yet supported by template haskell.
dec2TypeLevel :: Int -> Q Type
dec2TypeLevel n
 | n <  0 = error "natural number expected"
 | n < 10 = let name = case n of
                  0 -> ''D0; 1 -> ''D1; 2 -> ''D2; 3 -> ''D3; 4 -> ''D4
                  5 -> ''D5; 6 -> ''D6; 7 -> ''D7; 8 -> ''D8; 9 -> ''D9
            in conT name          
 | otherwise = let (quotient, reminder) = n `quotRem` 10 
                   remType  = dec2TypeLevel reminder
                   quotType = dec2TypeLevel quotient
               in (conT ''(:*)) `appT` quotType `appT` remType


-- | Generate a decimal type synonym alias
genTypeAlias :: String -> Q Type -> Q Dec
genTypeAlias str t = tySynD name [] t
 where name = mkName $ str

-- | Generate a decimal value-level reflected alias
genValAlias :: String -> Q Type -> Q Dec
genValAlias str t = body
 where name = mkName $ str
       body = valD (varP name) 
                   (normalB (sigE [| undefined |] t)) []


-- | Print an integer in certain base
toBase :: Base  -- base 
       -> Int  -- Number to print
       -> String
toBase Dec n = show n
toBase b n
  | n < 0 = '-' : toBase b (- n)
  | n < bi = [int2Char n]
  | otherwise = (toBase b rest) ++ [int2Char currDigit]
   where bi = base2Int b 
         (rest, currDigit) = n `quotRem` bi

-- | print the corresponding character of a digit
int2Char ::  Int  -- Number to print
          -> Char
int2Char i 
 | i' < 10 = toEnum (i'+ 48)
 | otherwise = toEnum (i' + 55)
 where i' = abs i
