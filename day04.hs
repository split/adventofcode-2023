module Main where

import Data.List (group, sort)
import Data.Bifunctor (first, second)

main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1 = ("Part 1: " ++) . show . sum . map ((2^) . pred . length) . filter (not . null)

part2 = ("Part 2: " ++) . show  . sum . map fst . copies . map ((1,) . length)

copies :: [(Int, Int)] -> [(Int, Int)]
copies  (card@(c, n) : xs) = card : copies (map (first (+ c)) (take n xs) ++ drop n xs)
copies _ = []

parse = filter ((>1) . length). group . sort . words