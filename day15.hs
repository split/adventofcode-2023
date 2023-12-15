module Main where

import Data.Char (ord)
import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1] . splitOn "," . head . lines)

part1 = ("Part 1: " ++) . show . sum . map hash

hash = foldl (\c v -> (c + v) * 17 `mod` 256) 0 . map ord