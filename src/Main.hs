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

import Topology (cycles)
import Path (path, pathFailed, broadcast, broadcastFailed)

import Data.List (intercalate)

-- render :: (Show a) => [(a, a)] -> String
-- render = intercalate ", " .  map (\(f,t) -> show f ++ " -> " ++ show t)
--
-- renders = intercalate "\n" . map render

main :: IO ()
main = print $ cycles 4
