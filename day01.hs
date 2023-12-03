module Main where
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (findIndex, isPrefixOf, tails)

main = interact (unlines . sequence [part1, part2] . lines)

part1 :: [String] -> String
part1 = ("Part 1: " ++) . show . sum . map (sumfl . filter isDigit)

part2 = ("Part 2: " ++) . show . sum . map (sumfl . mapMaybe readDigit . tails)

sumfl :: String -> Int
sumfl xs = read [head xs, last xs]

readDigit :: String -> Maybe Char
readDigit [] = Nothing
readDigit str@(c:_)
  | isDigit c = Just c
  | otherwise = head . show <$> findIndex (`isPrefixOf` str) digits


digits = ["0", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]