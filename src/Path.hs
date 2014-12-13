{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
--
-- Module      :  Path
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

module Path (
      path
    , Path
) where

import Prelude hiding (cycle, elem)

import Data.List ((\\), sortBy, intercalate)
import Data.Function (on)
import Data.Collections (elem, minimumBy, size)
import Data.Cycle (Cycle, goRight, goLeft, getValue, rightValue, leftValue)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid, (<>))

import Util (goTo)


newtype Path = Path { unPath :: [Int] } deriving (Monoid)

instance Show Path where
    show = intercalate " -> " . map show . unPath


bestPath :: [Path] -> Path
bestPath = minimumBy (compare `on` size . unPath)

pathCycle :: (Int, Int) -> Cycle Int -> Maybe Path
pathCycle (from, to) cycle =
    if from `elem` cycle && to `elem` cycle
        then Just p
        else Nothing
    where cycle' = goTo from cycle
          pathCycle' go = reverse . snd $ result
              where result = until reached step (cycle', [])
                    step (c, p') = (go c, getValue c : p')
                    reached (_, n : _) = n == to
                    reached (_, [])    = False
          lp = Path $ pathCycle' goLeft
          rp = Path $ pathCycle' goRight
          p = bestPath [lp, rp]

path :: (Int, Int) -> [Cycle Int] -> Path
path = path' $ Path []

path' :: Path -> (Int, Int) -> [Cycle Int] -> Path
path' res (from, to) cycles =
    case nears of
       [] -> path' (res <> Path [from]) (neighbor, to) cycles
       _  -> res <> bestPath nears
    where nears = mapMaybe (pathCycle (from, to)) cycles
          incycles = sortBy (compare `on` size) $ filter (from `elem`) cycles
          allneighbors = concatMap (getNeighbors . goTo from) incycles
          getNeighbors c = [rightValue c, leftValue c]
          neighbor = head (allneighbors \\ unPath res)
