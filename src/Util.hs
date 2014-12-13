-----------------------------------------------------------------------------
--
-- Module      :  Util
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

module Util (
      headMaybe
    , cshiftL
    , log2
    , slice
    , goTo
) where

import Data.Bits (shiftL, testBit, setBit, clearBit, Bits)
import Data.Maybe (listToMaybe)
import Data.Cycle (goRight, Cycle, getValue)

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


log2 :: Int -> Int
log2 n = ceiling $ logBase 2.0 (fromIntegral n :: Double)


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

goTo :: Eq a => a -> Cycle a -> Cycle a
goTo e = until ((== e) . getValue) goRight
