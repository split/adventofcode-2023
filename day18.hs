{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (ap)
import Data.List (group)

data Dir = R | D | L | U deriving (Show, Eq, Enum, Read, Bounded)

type Move = (Dir, Int)

main = interact (unlines . sequence [part1, part2] . map words . lines)

part1, part2 :: [[String]] -> String
part1 = ("Part 1: " ++) . show . calc . map readMove
part2 = ("Part 2: " ++) . show . calc . map (readHex . (!! 2))

calc = ap ((+) . border) (area . scanl move (0, 0) . map follow)
  where
    area xs = (`div` 2) . abs . sum $ zipWith (\(x1, y1) (x2, y2) -> (y1 + y2) * (x2 - x1)) xs (tail (cycle xs))
    border = (+ 1) . (`div` 2) . sum . map snd

follow :: Move -> (Int, Int)
follow = \case
  (R, n) -> (n, 0)
  (L, n) -> (-n, 0)
  (D, n) -> (0, n)
  (U, n) -> (0, -n)

readMove :: [String] -> Move
readMove (d : n : _) = (read d, read n)

readHex :: String -> Move
readHex xs = (toEnum (read [xs !! 7]), read ("0x" ++ take 5 (drop 2 xs)))

move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)