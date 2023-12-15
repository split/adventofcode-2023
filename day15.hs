{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (first)
import Data.Char (ord)
import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Map.Ordered (OMap, (|>))
import Data.Map.Ordered qualified as OMap

main = interact (unlines . sequence [part1, part2] . splitOn "," . head . lines)

part1 = ("Part 1: " ++) . show . sum . map hash

part2 = ("Part 2: " ++) . show . focusingPower . boxify

hash = foldl (\c v -> (c + v) * 17 `mod` 256) 0 . map ord

boxify =
  map (first ((+ 1) . hash))
    . OMap.assocs
    . foldl (go []) OMap.empty
  where
    go label boxes = \case
      ('=' : d) -> boxes OMap.|> (label, read d :: Int)
      ('-' : d) -> label `OMap.delete` boxes
      (c : xs) -> go (label ++ [c]) boxes xs

focusingPower [] = 0
focusingPower lenses@((h, v) : _) =
  let (hs, lenses') = partition ((== h) . fst) lenses
   in sum (zipWith (\h (s, f) -> h * s * f) [1 ..] hs) + focusingPower lenses'