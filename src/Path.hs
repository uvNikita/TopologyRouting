{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
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
    , broadcast
) where

import Prelude hiding (cycle, elem)

import Data.List ((\\), sortBy, intercalate, nubBy, nub)
import Data.Set (toList, union)
import Data.Function (on)
import Data.Collections (elem, minimumBy, size, fromList)
import Data.Cycle (Cycle, goRight, goLeft, getValue, rightValue, leftValue)
import Data.Maybe (mapMaybe, fromJust)
import Data.Monoid (Monoid, (<>))

import Control.Applicative ((<$>))

import Util (goTo, headMaybe)


newtype Path = Path { unPath :: [Int] } deriving (Monoid)

instance Show Path where
    show = intercalate " -> " . map show . unPath


bestPath :: [Path] -> Path
bestPath = minimumBy (compare `on` size . unPath)

neighbors :: Int -> [Cycle Int] -> [Int]
neighbors node = nub . concatMap (directNeighbors . goTo node) . filter (node `elem`)
    where directNeighbors c = [rightValue c, leftValue c]

pathCycle :: [Int] -> (Int, Int) -> Cycle Int -> Maybe Path
pathCycle failed (from, to) cycle =
    if from `elem` cycle && to `elem` cycle
        then p
        else Nothing
    where cycle' = goTo from cycle
          pathCycle' go = reverse . snd $ result
              where result = until reached step (cycle', [])
                    step (c, p') = (go c, getValue c : p')
                    reached (_, n : _) = n == to
                    reached (_, [])    = False
          lp = Path $ pathCycle' goLeft
          rp = Path $ pathCycle' goRight
          isFailed p = any (`elem` unPath p) failed
          p = case (isFailed lp, isFailed rp) of
                  (True,  True)  -> Nothing
                  (True,  False) -> Just rp
                  (False, True)  -> Just lp
                  (False, False) -> Just $ bestPath [lp, rp]

path :: (Int, Int) -> [Cycle Int] -> Path
path d c = fromJust $ path' (Path []) [] d c

pathFailed :: [Int] -> (Int, Int) -> [Cycle Int] -> Maybe Path
pathFailed = path' (Path [])

broadcast :: Int -> [Cycle Int] -> [[(Int, Int)]]
broadcast start cycles = step $ fromList [start]
    where step done = case new of
                        [] -> []
                        _ -> new : step done'
              where new = foldr add [] (toList done)
                    add n acc = case (neighbors n cycles \\ used) \\ toList done of
                                    [] -> acc
                                    neighbor : _ -> (n, neighbor) : acc
                                    where used = map snd acc
                    done' = done `union` fromList (map snd new)

path' :: Path -> [Int] -> (Int, Int) -> [Cycle Int] -> Maybe Path
path' res failed (from, to) cycles =
    case nears of
       [] -> case neighbor of
                 Nothing -> Nothing
                 Just n  -> path' (res <> Path [from]) failed (n, to) cycles
       _  -> Just $ res <> bestPath nears
    where nears = mapMaybe (pathCycle failed (from, to)) cycles
          incycles = sortBy (compare `on` size) $ filter (from `elem`) cycles
          neighbor = headMaybe ((neighbors from incycles \\ unPath res) \\ failed)
