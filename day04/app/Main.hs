module Main (main) where

import Control.Monad.Except (throwError)
import Data.List (intersect)
import Text.ParserCombinators.Parsec

-- Types
type Range = (Int, Int)

-- Parsing
parseRange :: Parser Range
parseRange = do
  low <- read <$> many1 digit
  _ <- char '-'
  high <- read <$> many1 digit
  return (low, high)

parsePair :: Parser (Range, Range)
parsePair = do
  one <- parseRange
  _ <- char ','
  two <- parseRange
  return (one, two)

parseAssignments :: Parser [(Range, Range)]
parseAssignments = parsePair `sepBy` newline

parseInput :: String -> Either String [(Range, Range)]
parseInput input = case parse parseAssignments "assignments" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
isCompletelyCovering :: (Int, Int) -> (Int, Int) -> Bool
isCompletelyCovering (x, y) (z, w) = interLen == len1 || interLen == len2
  where
    len1 = y - x + 1
    len2 = w - z + 1
    interLen = length $ intersect [x .. y] [z .. w]

hasOverlap :: (Int, Int) -> (Int, Int) -> Bool
hasOverlap (x, y) (z, w) = interLen > 0
  where
    interLen = length $ intersect [x .. y] [z .. w]

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right assignments) = parseInput input
  print $ sum $ boolToInt . uncurry isCompletelyCovering <$> assignments
  print $ sum $ boolToInt . uncurry hasOverlap <$> assignments
