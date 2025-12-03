module AoC2025.Day03 (partOne, partTwo) where

import Data.List.Extra (chunksOf, singleton)
import Data.String.Utils
import Text.Parsec (endOfLine)
import Text.ParserCombinators.Parsec
import Data.List (subsequences)
import Control.Concurrent.Async (mapConcurrently)

-- Types
type Cell = Int
type Bank = [Cell]

-- Parsing
parseSingleInt :: Parser Int
parseSingleInt = read . singleton <$> digit

parseBank :: Parser Bank
parseBank = many1 parseSingleInt

parseBanks :: Parser [Bank]
parseBanks = parseBank `sepBy` endOfLine

parseInput :: String -> Either String [Bank]
parseInput s = case parse parseBanks "lists" (rstrip s) of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
allOrderedPairs :: Bank -> [[Cell]]
allOrderedPairs [] = []
allOrderedPairs bank@(x:xs) = [[x, y] | y <- xs] <> allOrderedPairs xs

allBatteries :: Int -> Bank -> [[Cell]]
allBatteries 0 _ = [[]]
allBatteries _ [] = []
allBatteries n xs = filter ((== n) . length) $ subsequences xs

asJolts :: [Cell] -> Int
asJolts xs = read (concatMap show xs)

-- helper
safeMaximum :: [Int] -> Int
safeMaximum [] = 0
safeMaximum xs = maximum xs

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "Part One"
  let (Right banks) = parseInput input
  let results = map (safeMaximum . map asJolts . allOrderedPairs) banks
  print $ sum results

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "Part Two"
  let (Right banks) = parseInput input
  results <- mapConcurrently (\bank -> return $! safeMaximum $ map asJolts $ allBatteries 12 bank) banks
  print $ sum results
