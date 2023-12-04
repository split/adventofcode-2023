module Main where

import Data.List (group, sort)

main = interact (unlines . sequence [part1] . map parse . lines)

part1 = ("Part 1: " ++) . show . sum . map ((2^) . pred . length) . filter (not . null)

parse = filter ((>1) . length). group . sort . words