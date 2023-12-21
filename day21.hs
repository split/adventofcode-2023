module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.Foldable (foldl')
import Data.List (transpose)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Debug.Trace (trace)

type Pedometer = Map Plot (Set Int)

type Garden = Set Plot

type Plot = (Int, Int)

main = interact (unlines . sequence [part1] . graph . lines)

part1 = ("Part 1: " ++) . show . length . uncurry (walkUntil 64)

walkUntil n garden pedometer = getPlots n $ foldl (flip (visit garden)) pedometer [1 .. n]

visit :: Garden -> Int -> Pedometer -> Pedometer
visit garden step = ap (foldr (Map.alter set)) (Set.intersection garden . plots)
  where
    plots = Set.unions . Set.map dirs . getPlots (step - 1)
    set :: Maybe (Set Int) -> Maybe (Set Int)
    set state = do
      visits <- state <|> return Set.empty
      return $ Set.insert step visits

getPlots step = Map.keysSet . Map.filter (Set.member step)

graph :: [String] -> (Garden, Pedometer)
graph input =
  (Map.keysSet &&& Map.map (const $ Set.singleton 0) . Map.filter (== 'S')) $
    Map.fromList [((x, y), v) | (y, row) <- zip [0 ..] input, (x, v) <- zip [0 ..] row, v /= '#']

dirs (x, y) = Set.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]