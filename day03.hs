module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isDigit)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (find, nub)

data Point = Part String | Symbol Char deriving (Show, Eq)
data Schematic = Map (Int, Int) Point

main = interact (unlines . sequence [part1, part2] . lines)

part1 = ("Part 1: " ++ ) . show . sum . nub . map countSymbols . findAdjacent
  where
    countSymbols (Part pid, adj) = if any isSymbol adj then read pid else 0
    countSymbols _ = 0

part2 = ("Part 2: " ++ ) . show . sum . map gearRatio . findAdjacent
  where
    gearRatio (Symbol '*', [Part p1, Part p2]) = read p1 * read p2
    gearRatio _ = 0

findAdjacent input = Map.elems $ Map.mapWithKey solve schematic
  where
    schematic = toSchematic input
    solve xy point = (point, nub $ mapMaybe (schematic Map.!?) (adjacents xy len))
      where
        len = case point of 
          Part pid -> length pid
          _ -> 1

adjacents :: (Int, Int) -> Int -> [(Int, Int)]
adjacents (x, y) len = [(x+dx, y+dy) | dx <- [(-1)..len], dy <- [-1..1], not (dy == 0 && (dx >= 0 && dx < len))]

toSchematic input = Map.fromList [((x, y), c) | (y, row) <- zip [0..] input, (x, c) <- toPoint row 0]

toPoint :: String -> Int -> [(Int, Point)]
toPoint [] _ = []
toPoint str@(c:_) x = zip [x..] points ++ toPoint (drop len str) (x + len)
  where
    len = max (length points) 1
    points
      | isDigit c = let pid = takeWhile isDigit str in map (const (Part pid)) pid
      | c == '.'  = []
      | otherwise = [Symbol c]

isSymbol x = case x of 
  (Symbol _) -> True
  _ -> False