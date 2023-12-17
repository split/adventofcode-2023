module Main where

import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Heap qualified as H
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe (mapMaybe, maybe)
import Data.Set qualified as S

type Lavafall = Map (Int, Int) Int

type State = ((Int, Int), (Int, Int), Int)

main = interact (unlines . sequence [part1, part2] . parseLavafall . lines)

part1 = ("Part 1: " ++) . maybe "Not found" show . travel (const (<= 3))

part2 = ("Part 2: " ++) . maybe "Not found" show . travel (\n n' -> if n' <= n then n >= 4 else n' <= 10)

cost lavafall heat rule (block, dir, n) dir' = do
  heat' <- lavafall !? block'
  guard (rule n n' && dir' /= opposite dir)
  return (heat + heat', (block', dir', n'))
  where
    block' = block `move` dir'
    n' = if dir' == dir then n + 1 else 1

travel :: (Int -> Int -> Bool) -> Lavafall -> Maybe Int
travel rule lavafall = dijkstra initialHeap initialDistances
  where
    start = ((0, 0), (0, 0), 0)
    end = maximum $ M.keys lavafall
    initialHeap = H.fromList [(0, start)]
    initialDistances = M.empty
    dijkstra heap dists
      | null heap = return $ minimum $ M.filterWithKey (\(k, _, _) _ -> k == end) dists
      | otherwise = do
          ((heat, state), heap') <- H.uncons heap
          if state `M.member` dists
            then dijkstra heap' dists
            else
              let neighbors = H.fromList $ mapMaybe (cost lavafall heat rule state) dirs
               in dijkstra (heap' <> neighbors) (M.insertWith min state heat dists)

parseLavafall :: [String] -> Lavafall
parseLavafall input = M.fromList [((x, y), digitToInt h) | (y, row) <- zip [0 ..] input, (x, h) <- zip [0 ..] row]

opposite (x, y) = (negate x, negate y)

move (x, y) (dx, dy) = (x + dx, y + dy)

dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]
