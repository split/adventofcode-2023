{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (ap, foldM)
import Data.Map (Map, (!), (!?))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Loc = (Int, Int)

data TrailLog = TrailLog
  { out :: Bool,
    dp :: Map Loc Int,
    visited :: Set Loc,
    trails :: Map Loc Char
  }
  deriving (Show)

main = interact (unlines . sequence [part1] . parseTrails . lines)

part1 = ("Part 1: " ++) . show . maximum . dp . ap walkTrail (head . Map.keys . trails) . TrailLog False Map.empty Set.empty

walkTrail :: TrailLog -> Loc -> TrailLog
walkTrail log@(TrailLog {..}) loc
  | loc `Set.member` visited = log
  | otherwise = ways $ log {visited = Set.insert loc visited, dp = Map.insert loc 0 dp}
  where
    end = maximum $ Map.keys trails
    ways log = foldl follow log (slopes loc)

    update loc' log'@(TrailLog {..}) =
      if out || loc' == end then log' {dp = Map.insertWith max loc (dp ! loc' + 1) dp, out = True} else log'

    follow log (slope, loc') = case trails !? loc' of
      Just c | c == '.' || c == slope -> update loc' (walkTrail log loc')
      _ -> log

slopes (x, y) = [('v', (x, y + 1)), ('>', (x + 1, y)), ('^', (x, y - 1)), ('<', (x - 1, y))]

parseTrails input = Map.fromList [((x, y), c) | (y, row) <- zip [0 ..] input, (x, c) <- zip [0 ..] row, c /= '#']
