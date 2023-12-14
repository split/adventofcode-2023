module Main where

import Data.List (intercalate, partition, transpose)
import Data.List.Split (splitOn)
import Data.Set qualified as Set
import Data.Tuple (swap)

data Tilt = N | E | S | W deriving (Ord, Eq, Enum, Show)

main = interact (unlines . sequence [part1, part2] . lines)

part1, part2 :: [String] -> String
part1 = ("Part 1: " ++) . show . load . gravity N
part2 =
  ("Part 2: " ++)
    . maybe "Cycle not found" (show . load)
    . spinCycle 1000000000
    . iterate (flip (foldl (flip gravity)) [N .. W])

gravity :: Tilt -> [String] -> [String]
gravity tilt = vt . map (intercalate "#" . map (uncurry (++) . ht . partition (== '.')) . splitOn "#") . vt
  where
    (vt, ht) = case tilt of
      N -> (transpose, swap)
      W -> (id, id)
      S -> (transpose, id)
      E -> (id, swap)

spinCycle :: Int -> [[String]] -> Maybe [String]
spinCycle target platforms = do
  start <- findDuplicate platforms
  repeating <- findDuplicate (drop start platforms)
  return $ platforms !! (start + (target - start) `mod` repeating)

load :: [String] -> Int
load = sum . concatMap (zipWith (\l b -> if b == 'O' then l else 0) [1 ..] . reverse) . transpose

findDuplicate :: (Eq a, Ord a) => [a] -> Maybe Int
findDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `Set.member` seen = Just (Set.size seen)
      | otherwise = go (Set.insert x seen) xs