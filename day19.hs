{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Bifunctor (bimap, first, second)
import Data.Either (fromRight)
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, many, runParser, sepBy, some, try)
import Text.Megaparsec.Char (char, eol, lowerChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Op
  = Call String
  | Gt Char Int Op
  | Lt Char Int Op
  | Accept
  | Reject
  deriving (Eq, Show)

data Mem = Mem
  { workflows :: Map String [Op],
    vars :: Map Char Int
  }
  deriving (Show)

main = interact (unlines . sequence [part1, part2] . fromRight [] . runParser parser "")

-- part1 :: [String] -> [Char]
part1 = ("Part 1: " ++) . show . sum . mapMaybe (`runOp` [Call "in"])

part2 = ("Part 2: " ++) . show . (`runBounds` [Call "in"]) . workflows . head

runOp :: Mem -> [Op] -> Maybe Int
runOp mem@Mem {..} = \case
  (Accept : _) -> return (sum vars)
  (Reject : _) -> Nothing
  (Call name : _) -> workflows !? name >>= runOp mem
  (Gt var limit op : rest) -> vars !? var >>= \val -> runOp mem (if val > limit then [op] else rest)
  (Lt var limit op : rest) -> vars !? var >>= \val -> runOp mem (if val < limit then [op] else rest)

-- a                                  b
--         A       >
-- a               b
--       R   <
--           a     b
--      >
--           a     b

runBounds :: Map String [Op] -> [Op] -> Int
runBounds workflows = go $ M.fromList ((,(1, 4000)) <$> "xmas")
  where
    go state = \case
      (Accept : _) -> foldr (\(start, end) sum -> sum * (end - start + 1)) 1 state
      (Reject : _) -> 0
      (Call name : _) -> go state (workflows ! name)
      (Gt var limit op : rest)
        | fst (state ! var) < limit ->
            go (M.adjust (first (max (limit + 1))) var state) [op]
              + go (M.adjust (second (min limit)) var state) rest
      (Lt var limit op : rest)
        | limit < snd (state ! var) ->
            go (M.adjust (second (min (limit - 1))) var state) [op]
              + go (M.adjust (first (max limit)) var state) rest
      (_ : rest) -> go state rest

-- Parsing of the input
type Parser = Parsec Void String

parser :: Parser [Mem]
parser = map . Mem <$> workflows <* eol <*> vars
  where
    workflows = M.fromList <$> some ((,) <$> some lowerChar <*> block parseOp)
    vars = map M.fromList <$> some (block var)
    var = (,) <$> lowerChar <* char '=' <*> decimal
    block chunk = between (char '{') (char '}') (chunk `sepBy` char ',') <* eol

parseOp :: Parser Op
parseOp = accept <|> reject <|> try gt <|> try lt <|> call
  where
    accept = Accept <$ char 'A'
    reject = Reject <$ char 'R'
    gt = Gt <$> lowerChar <* char '>' <*> decimal <* char ':' <*> parseOp
    lt = Lt <$> lowerChar <* char '<' <*> decimal <* char ':' <*> parseOp
    call = Call <$> some lowerChar
