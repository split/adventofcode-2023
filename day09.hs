module Main where

main = interact (unlines . sequence [part1] . map (map read . words) . lines)

part1 :: [[Int]] -> [Char]
part1 = ("Part 1: " ++) . show . sum . map extrapolate

extrapolate :: [Int] -> Int
extrapolate history = let dt = diffs history in 
  last history + if all (== 0) dt then last dt else extrapolate dt

diffs (x1:x2:xs) = x2 - x1 : diffs (x2:xs)
diffs _ = []