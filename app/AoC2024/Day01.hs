module AoC2024.Day01 (partOne, partTwo) where

import Text.ParserCombinators.Parsec
import qualified Data.List as L
import Data.List.Utils
import Data.Tuple.Utils

-- Parsing
parseInt :: Parser Int
parseInt = read <$> many1 digit

parseLine :: Parser [Int]
parseLine = do
  x <- parseInt
  _ <- many1 space
  y <- parseInt
  return [x, y]

parseLists :: Parser [[Int]]
parseLists = parseLine `sepBy` char '\n'

parseInput :: String -> Either String [[Int]]
parseInput s = case parse parseLists "lists" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "Part One"
  let (Right [xs, ys]) = fmap L.transpose <$> parseInput $ init input
  let result = sum $ abs <$> zipWith (-) (L.sort xs) (L.sort ys)
  print result

partTwo :: String -> IO ()
partTwo input = do
  let (Right [xs, ys]) = fmap L.transpose <$> parseInput $ init input
  let result = sum ((uncurry (*) <$> fmap (`countElem` ys)) . dup <$> xs)
  print result

