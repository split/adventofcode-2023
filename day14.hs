module Main where

import Control.Monad (ap)
import Data.List (intercalate, partition, transpose)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Debug.Trace (trace)

main = interact (unlines . sequence [part1] . lines)

part1 = ("Part 1: " ++) . show . load . map gravity . (!! 3) . rotations

gravity :: String -> String
gravity = intercalate "#" . map (uncurry (++) . swap . partition (== '.')) . splitOn "#"

load :: [String] -> Int
load = sum . concatMap (zipWith (\l b -> if b == 'O' then l else 0) [1 ..] . reverse)

rotations = iterate transpose