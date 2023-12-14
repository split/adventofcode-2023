module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.List (group, inits, sort, tails, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

main = interact (unlines . sequence [part1, part2] . map lines . splitOn "\n\n")

part1, part2 :: [[String]] -> String
part1 = ("Part 1: " ++) . show . sum . mapMaybe (summarize and)
part2 = ("Part 2: " ++) . show . sum . mapMaybe (summarize smudge)

summarize rule mirror = mirrorReflection rule (transpose mirror) <|> ((* 100) <$> mirrorReflection rule mirror)

smudge = (== 1) . length . filter not

mirrorReflection :: (Eq a) => ([Bool] -> Bool) -> [[a]] -> Maybe Int
mirrorReflection rule = go []
  where
    go _ [] = Nothing
    go refl (x : xs)
      | null xs = Nothing
      | rule $ zipWith (==) (concat (x : refl)) (concat xs) = return (length refl + 1)
      | otherwise = go (x : refl) xs
