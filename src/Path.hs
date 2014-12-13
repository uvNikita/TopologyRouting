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

import Data.List ((\\), sortBy)
import Data.Function (on)
import Data.Collections (elem, minimumBy, size)
import Data.Cycle (Cycle, goRight, goLeft, getValue, rightValue, leftValue)
import Data.Maybe (mapMaybe)

import Util (goTo)


type Path = [Int]


bestPath :: [Path] -> Path
bestPath = minimumBy (compare `on` length)

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
          lp = pathCycle' goLeft
          rp = pathCycle' goRight
          p = bestPath [lp, rp]

path :: (Int, Int) -> [Cycle Int] -> Path
path = path' []

path' :: [Int] -> (Int, Int) -> [Cycle Int] -> [Int]
path' res (from, to) cycles =
    case nears of
        [] -> path' (res ++ [from]) (neighbor, to) cycles
        _  -> res ++ bestPath nears
    where nears = mapMaybe (pathCycle (from, to)) cycles
          incycles = sortBy (compare `on` size) $ filter (from `elem`) cycles
          allneighbors = concatMap (getNeighbors . goTo from) incycles
          getNeighbors c = [rightValue c, leftValue c]
          neighbor = head (allneighbors \\ res)
