{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, insert, (!?))
import Data.Map.Strict qualified as Map

data Module
  = Broadcaster [String]
  | FlipFlop Bool [String]
  | Conjunction (Map String Bool) [String]
  deriving (Eq, Show)

data Pulse = Pulse Bool String String deriving (Eq)

type Wires = Map String Module

main = interact (unlines . sequence [part1] . parse)

part1 =
  ("Part 1: " ++)
    . show
    . (\(a, b) -> length a * length b)
    . partition hi
    . concatMap (map fst)
    . take 1000
    . simulate

simulate wires =
  let pulses = run [(pressButton, wires)]
   in pulses : simulate (snd $ last pulses)

pressButton = Pulse False "button" "roadcaster"

run :: [(Pulse, Wires)] -> [(Pulse, Wires)]
run = \case
  [] -> []
  (state@(Pulse phi from key, wires) : next) ->
    state : case wires !? key of
      Just (Broadcaster targets) ->
        run' wires (Pulse phi key <$> targets)
      Just (FlipFlop fhi targets)
        | not phi ->
            run'
              (insert key (FlipFlop (not fhi) targets) wires)
              (Pulse (not fhi) key <$> targets)
      Just (Conjunction mem targets) ->
        let mem' = insert from phi mem
         in run'
              (insert key (Conjunction mem' targets) wires)
              (Pulse (not $ and mem') key <$> targets)
      input -> run next
    where
      run' wires pulses = run ((,wires) <$> (fst <$> next) <> pulses)

hi :: Pulse -> Bool
hi (Pulse hi _ _) = hi

parse :: String -> Wires
parse = parseModules . map (parts . splitOn " -> ") . lines
  where
    parts [t : wire, targets] = (wire, (t, splitOn ", " targets))

parseModules input = Map.fromList $ map modu input
  where
    connect wire = Map.fromList $ (,False) . fst <$> filter (elem wire . snd . snd) input
    modu = \case
      (wire, ('%', targets)) -> (wire, FlipFlop False targets)
      (wire, ('&', targets)) -> (wire, Conjunction (connect wire) targets)
      (wire, ('b', targets)) -> (wire, Broadcaster targets)

-- To match format of the examples
instance Show Pulse where
  show (Pulse hi from to) = from ++ " -" ++ (if hi then "high" else "low") ++ "-> " ++ to
