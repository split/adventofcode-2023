module Main where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)

data Game = Game Int [Cubes] deriving (Show)
type Cubes = Map String Int

main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1 :: [Game] -> String
part1 = ("Part 1: " ++) . show . sum . map gid . filter (possible bag)
-- only 12 red cubes, 13 green cubes, and 14 blue cubes
  where bag = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

part2 = ("Part 2: " ++) . show . sum . map (product . fewest)
  where
    fewest (Game _ hands) = Map.unionsWith max hands

possible :: Cubes -> Game -> Bool
possible bag (Game n hands) = all (all (>= 0) . Map.unionWith (+) bag . Map.map negate) hands

parse :: String -> Game
parse s = Game (read (last $ words title)) hands
    where
      [title, game] = splitOn ": " s
      hands = map parseGame $ splitOn "; " game
      parseGame = Map.fromList . map parseHand . splitOn ", "
      parseHand hand = case words hand of
        [count, cube] -> (cube, read count)
        _ -> error ("Invalid input: " ++ hand)

gid :: Game -> Int
gid (Game n _) = n