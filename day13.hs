module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.List (inits, tails, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

main = interact (unlines . sequence [part1] . map lines . splitOn "\n\n")

part1 = ("Part 1: " ++) . show . sum . mapMaybe summarize

summarize mirror = mirrorReflection (transpose mirror) <|> ((* 100) <$> mirrorReflection mirror)

mirrorReflection :: (Eq a) => [[a]] -> Maybe Int
mirrorReflection = go 1 []
  where
    go _ _ [] = Nothing
    go i refl (x : xs)
      | null xs = Nothing
      | and $ zipWith (==) (x : refl) xs = return i
      | otherwise = go (i + 1) (x : refl) xs
