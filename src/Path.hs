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
      Path
    , path
    , pathFailed
) where

import Prelude hiding (cycle, elem)

import Data.List ((\\), sortBy, intercalate)
import Data.Function (on)
import Data.Collections (elem, minimumBy, size)
import Data.Cycle (Cycle, goRight, goLeft, getValue, rightValue, leftValue)
import Data.Maybe (mapMaybe, fromJust)
import Data.Monoid (Monoid, (<>))

import Util (goTo, headMaybe)


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
path d c = fromJust $ path' (Path []) [] d c

pathFailed :: [Int] -> (Int, Int) -> [Cycle Int] -> Maybe Path
pathFailed = path' (Path [])

path' :: Path -> [Int] -> (Int, Int) -> [Cycle Int] -> Maybe Path
path' res failed (from, to) cycles =
    case nears of
       [] -> case neighbor of
                 Nothing -> Nothing
                 Just n  -> path' (res <> Path [from]) failed (n, to) cycles
       _  -> Just $ res <> bestPath nears
    where nears = mapMaybe (pathCycle (from, to)) cycles
          incycles = sortBy (compare `on` size) $ filter (from `elem`) cycles
          allneighbors = concatMap (getNeighbors . goTo from) incycles
          getNeighbors c = [rightValue c, leftValue c]
          neighbor = headMaybe ((allneighbors \\ unPath res) \\ failed)
