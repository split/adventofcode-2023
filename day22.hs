module Main where

import Control.Arrow (ArrowChoice (left), (&&&))
import Control.Monad (ap)
import Data.Bifunctor (bimap, first, second)
import Data.Either (isLeft, lefts, rights)
import Data.List (nub, sortBy, (\\))
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Debug.Trace (trace)

data V3 = V3 {x :: Int, y :: Int, z :: Int} deriving (Show, Eq)

type Block = (Char, (V3, V3))

main = interact (unlines . sequence [part1] . map return . sortBy (comparing (minZ . snd)) . zipWith parse ['A' ..] . lines)

part1 = ("Part 1: " ++) . show . disintegrated . stopped . last . dropBlocks

disintegrated :: [(Block, [Block])] -> Int
disintegrated = ap ((-) . length) (length . supporters)

supporters = nub . concat . filter ((== 1) . length) . map (map fst . snd)

dropBlocks :: [Either (Block, [Block]) Block] -> [[Either (Block, [Block]) Block]]
dropBlocks blocks
  | null (moving blocks) = []
  | otherwise = droppedBlocks : dropBlocks droppedBlocks
  where
    droppedBlocks =
      foldl
        (\blocks' block -> blocks' <> [dropBlock (fst <$> stopped blocks') block])
        (filter isLeft blocks)
        (moving blocks)

dropBlock :: [Block] -> Block -> Either (Block, [Block]) Block
dropBlock stopped block@(name, (c1, c2))
  | not $ null blocking = Left (block, blocking)
  | min (z c1) (z c2) > 1 = return moved
  | otherwise = Left (block, [])
  where
    blocking = filter (moved `occupies`) stopped
    moved = (name, (c1 {z = z c1 - 1}, c2 {z = z c2 - 1}))

occupies :: Block -> Block -> Bool
occupies (_, corners) (_, block) = contains corners block

{- ORMOLU_DISABLE -}
contains :: (V3, V3) -> (V3, V3) -> Bool
contains (a, b) (c, d) =
    x c <= x b && x d >= x a &&
    y c <= y b && y d >= y a &&
    z c <= z b && z d >= z a
{- ORMOLU_ENABLE -}

minZ :: (V3, V3) -> Int
minZ (a, b) = min (z a) (z b)

without :: [a] -> [[a]]
without [] = []
without (x : xs) = xs : ((x :) <$> without xs)

parse :: Char -> String -> Block
parse name =
  (\[c1, c2] -> (name, (c1, c2)))
    . ((\[x, y, z] -> V3 x y z) . (read <$>) . splitOn "," <$>)
    . splitOn "~"

rem1 (V3 x y z) = V3 (x - 5) (y - 5) (z - 5)

moving = rights

stopped = lefts
