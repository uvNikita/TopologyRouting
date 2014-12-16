-----------------------------------------------------------------------------
--
-- Module      :  Topology
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Topology (
    cycles
) where

import Prelude hiding (cycle, elem)

import Data.Bits (bit, xor)
import Data.List ((\\))
import Data.Collections (fromList, elem)
import Data.Cycle (Cycle)

import Util (headMaybe, cshiftL, log2)


type GenFunc = Int -> Int

generate :: Eq a => (a -> a) -> [a] -> [a]
generate f ls@(l:_) =
    if new `elem` ls then ls else generate f (new : ls)
    where new = f l
generate _ [] = error "Must provide init value for generate"

cycle :: Int -> GenFunc -> [Cycle Int]
cycle n f = map fromList $ step [] 0
    where allNums = [0 .. 2 ^ n - 1]
          step res init = case findInit of
                              Nothing -> res'
                              Just init' -> step res' init'
              where new = generate f [init]
                    res' = new : res
                    findInit = headMaybe $ allNums \\ concat res'

genFuncs :: Int -> [GenFunc]
genFuncs pow = map (genFunc pow) [1 .. n]
    where n = log2 pow

genFunc :: Int -> Int -> GenFunc
genFunc pow i = cshiftL pow i . xor (if odd i then f else l)
    where f = 1
          l = bit (pow - 1)

cycles :: Int -> [Cycle Int]
cycles pow = concatMap (cycle pow) $ genFuncs pow
