module Main where

import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.List (group, groupBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)

type Graph = Map (Int, Int) Char

type PosDir = ((Int, Int), (Int, Int))

main = interact (unlines . sequence [part1] . graph . lines)

part1 :: Graph -> String
part1 =
  ("Part 1: " ++)
    . show
    . maximum
    . map length
    . mapMaybe (detect [] . tail . uncurry (iterate . traversePipe))
    . findStarts

detect :: [[PosDir]] -> [[PosDir]] -> Maybe [[PosDir]]
detect path [] = Nothing
detect path (posdirs : xs) = case length (group (map fst posdirs)) of
  0 -> Nothing
  1 -> return (posdirs : path)
  _ -> detect (posdirs : path) xs

traversePipe :: Graph -> [PosDir] -> [PosDir]
traversePipe graph = mapMaybe (step graph)

step :: Graph -> PosDir -> Maybe PosDir
step graph (pos, dir) = do
  let pos' = move pos dir
  pipe <- graph Map.!? pos'
  flowDir <- flow pipe dir
  return (pos', flowDir)

flow :: Char -> (Int, Int) -> Maybe (Int, Int)

-- | is a vertical pipe connecting north and south.
flow '|' (0, -1) = return (0, -1)
flow '|' (0, 1) = return (0, 1)
-- - is a horizontal pipe connecting east and west.
flow '-' (1, 0) = return (1, 0)
flow '-' (-1, 0) = return (-1, 0)
-- L is a 90-degree bend connecting north and east.
flow 'L' (0, 1) = return (1, 0)
flow 'L' (-1, 0) = return (0, -1)
-- J is a 90-degree bend connecting north and west.
flow 'J' (0, 1) = return (-1, 0)
flow 'J' (1, 0) = return (0, -1)
-- 7 is a 90-degree bend connecting south and west.
flow '7' (0, -1) = return (-1, 0)
flow '7' (1, 0) = return (0, 1)
-- F is a 90-degree bend connecting south and east.
flow 'F' (0, -1) = return (1, 0)
flow 'F' (-1, 0) = return (0, 1)
-- When pipes just don't connect.
flow _ _ = Nothing

findStarts :: Graph -> [(Graph, [PosDir])]
findStarts graph =
  [ (Map.insert pos s graph, (pos,) <$> mapMaybe (flow s) dirs)
    | (pos, v) <- Map.toList graph,
      v == 'S',
      s <- "|-LJ7F"
  ]

graph :: [String] -> Graph
graph input = Map.fromList [((x, y), pipe) | (y, row) <- zip [0 ..] input, (x, pipe) <- zip [0 ..] row, pipe /= '.']

move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dirs = [(1, 0), (0, -1), (-1, 0), (0, 1)]