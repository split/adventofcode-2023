module Main where

import Control.Arrow ((&&&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, intersection, (\\))
import Data.Set qualified as Set
import Debug.Trace (trace)

type Garden = Set Plot

type Plot = (Int, Int)

main = interact (unlines . sequence [part1] . graph . lines)

part1 = ("Part 1: " ++) . show . length . (!! 64) . uncurry (iterate . walk)

walk = flip $ intersection . Set.unions . Set.map dirs

graph input =
  (Map.keysSet &&& Map.keysSet . Map.filter (== 'S')) $
    Map.fromList [((x, y), v) | (y, row) <- zip [0 ..] input, (x, v) <- zip [0 ..] row, v /= '#']

dirs (x, y) = Set.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]