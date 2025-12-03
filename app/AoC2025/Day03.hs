module AoC2025.Day03 (partOne, partTwo) where

import Data.Array qualified as A
import Data.List.Extra (singleton)
import Data.String.Utils
import Debug.Trace (trace)
import Text.Parsec (endOfLine)
import Text.ParserCombinators.Parsec

-- Types
type Cell = Int

type Bank = [Cell]

type UpToIndex = Int

type NumChosen = Int

type DynArray = A.Array (UpToIndex, NumChosen) Cell

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
allOrderedPairs (x : xs) = [[x, y] | y <- xs] <> allOrderedPairs xs

asJolts :: [Cell] -> Int
asJolts xs = read (concatMap show xs)

safeMaximum :: [Int] -> Int
safeMaximum [] = 0
safeMaximum xs = maximum xs

combineCells :: Cell -> Cell -> Cell
combineCells a b = read (show a <> show b)

maxFromNCells :: Int -> Bank -> Cell
maxFromNCells n bank = finish $ foldl iter startState pairs
  where
    pairs =
      [ (upTo, chosen)
        | upTo <- [0 .. length bank],
          chosen <- [0 .. n]
      ]
    startState = A.array ((0, 0), (length bank, n)) [((i, j), 0) | i <- [0 .. length bank], j <- [0 .. n]]
    iter state (0, _) = state
    iter state (_, 0) = state
    iter state (upTo, chosen) = if chosen > upTo then state else state A.// [((upTo, chosen), max valueSkip valueTake)]
      where
        valueSkip = state A.! (upTo - 1, chosen)
        valueTake = combineCells (state A.! (upTo - 1, chosen - 1)) (bank !! (upTo - 1))
    finish state = state A.! (length bank, n)

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
  let cs = maxFromNCells 12 <$> banks
  print $ sum cs
