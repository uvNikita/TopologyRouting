-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Data.Bits (bit, xor, shiftL, zeroBits, (.&.), testBit, setBit, clearBit, Bits)
import Data.Maybe (listToMaybe)
import Data.List ((\\))


type GenFunc = Int -> Int

headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

cshiftL :: (Bits a) => Int -> Int -> a -> a
cshiftL _ 0 n = n
cshiftL pow shift n = cshiftL pow (shift - 1) n'
    where shifted = shiftL n 1
          cshifted = if testBit shifted pow
                         then setBit shifted 0
                         else shifted
          n' = clearBit cshifted pow

generate :: Eq a => (a -> a) -> [a] -> [a]
generate f ls@(l:_) =
    if new `elem` ls then ls else generate f (new : ls)
    where new = f l

circle :: Int -> GenFunc -> [[Int]]
circle n f = step [] 0
    where all = [0 .. 2 ^ n - 1]
          step res init = case findInit of
                              Nothing -> res'
                              Just init' -> step res' init'
              where new = generate f [init]
                    res' = new : res
                    findInit = headMaybe $ all \\ concat res'

log2 :: Int -> Int
log2 n = ceiling $ logBase 2 (fromIntegral n)

genFuncs :: Int -> [GenFunc]
genFuncs pow = map (genFunc pow) [1 .. n]
    where n = log2 pow

genFunc :: Int -> Int -> GenFunc
genFunc pow i = cshiftL pow i . xor (if odd i then first else last)
    where first = 1
          last  = bit (pow - 1)

circles :: Int -> [[[Int]]]
circles pow = map (circle pow) $ genFuncs pow

main = undefined
