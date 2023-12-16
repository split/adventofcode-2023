{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (ap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Tuple (swap)

type V2 = (Int, Int)

type Contraption = Map V2 (V2 -> [V2])

main = interact (unlines . sequence [part1, part2] . lines)

part1, part2 :: [String] -> String
part1 = ("Part 1: " ++) . show . beamEnergy ((-1, 0), (1, 0)) . toContraption
part2 = ("Part 2: " ++) . show . ap (maxEnergy . toContraption) toEdges
  where
    maxEnergy contraption = maximum . map (`beamEnergy` contraption)

beamEnergy beam = Set.size . Set.map fst . flip energize (Set.singleton beam) . energizeTile

energize energizeTile' = go Set.empty
  where
    go energized beams
      | null beams = energized
      | otherwise =
          let beams' = Set.fromList $ concatMap (energizeTile' . move) beams
           in go (energized <> beams') (beams' \\ energized)

energizeTile :: Contraption -> (V2, V2) -> [(V2, V2)]
energizeTile contraption (tile, dir) = case contraption Map.!? tile of
  Just enc -> (tile,) <$> enc dir
  _ -> []

encounter :: (Num a, Eq a) => Char -> (a, a) -> [(a, a)]
encounter = \case
  '.' -> return
  '\\' -> return . swap
  '/' -> return . both negate . swap
  '-' -> \case
    p@(_, 0) -> return p
    _ -> [(-1, 0), (1, 0)]
  '|' -> \case
    p@(0, _) -> return p
    _ -> [(0, -1), (0, 1)]
  _ -> const []

move ((x, y), (dx, dy)) = ((x + dx, y + dy), (dx, dy))

toContraption :: [String] -> Contraption
toContraption input = Map.fromList [((x, y), encounter tile) | (y, row) <- zip [0 ..] input, (x, tile) <- zip [0 ..] row]

toEdges :: [String] -> [(V2, V2)]
toEdges input =
  [((x, -1), (0, 1)) | x <- [0 .. (w - 1)]]
    ++ [((x, h), (0, -1)) | x <- [0 .. (w - 1)]]
    ++ [((-1, y), (1, 0)) | y <- [0 .. (h - 1)]]
    ++ [((w, y), (-1, 0)) | y <- [0 .. (h - 1)]]
  where
    h = length input
    w = length (head input)

both f (a, b) = (f a, f b)