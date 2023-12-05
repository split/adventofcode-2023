{-# LANGUAGE RecordWildCards #-}
module Main where
import Data.List.Split (splitOn)
import Control.Monad (msum, ap)
import Data.Maybe (mapMaybe, fromMaybe)
import Debug.Trace (trace)
import Data.Bifunctor (first, second)

data Range = Range { destStart :: Int, sourceStart :: Int, range :: Int } deriving (Show)

main = interact (unlines . sequence [part1] . parse . splitOn "\n\n")

part1 = ("Part 1: " ++) . show . minimum . uncurry (foldl pickRange)

pickRange :: [Int] -> [Range] -> [Int]
pickRange sources ranges = map (\s -> fromMaybe s (msum $ map (`applyRange` s) ranges)) sources 

applyRange :: Range -> Int -> Maybe Int
applyRange Range{..} source =
  if sourceStart <= source && source <= sourceStart + range
    then return (source + destStart - sourceStart)
    else Nothing

parse :: [String] -> ([Int], [[Range]])
parse (x:xs) = (seeds, ranges)
  where
    seeds = map read $ tail $ words x
    ranges = map (map (toRange . map read . words) . tail . lines) xs
    toRange [destStart, sourceStart, range] = Range destStart sourceStart range
