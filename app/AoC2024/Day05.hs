module AoC2024.Day05 (partOne, partTwo) where

import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)
import Data.List (sortBy)

-- Types
type Rule = (Int, Int)
type Update = [Int]
type PrinterProgram = ([Rule], [Update])

-- Parsing
number :: Parser Int
number = read <$> many1 digit

rule :: Parser Rule
rule = do
  x <- number
  _ <- char '|'
  y <- number
  return (x, y)

updates :: Parser Update
updates = number `sepBy` char ','

printerProgram :: Parser PrinterProgram
printerProgram = do
  os <- try rule `endBy` endOfLine
  _ <- newline
  us <- updates `sepBy` newline
  return (os, us)

parseInput :: String -> Either String PrinterProgram
parseInput s = case parse printerProgram "printer" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
sortByRules :: [Rule] -> Update -> Update
sortByRules rs = sortBy f
  where
    f x y = if (x, y) `elem` rs then LT else GT

isSorted :: [Rule] -> Update -> Bool
isSorted rs us = us == sortByRules rs us

middlePage :: Update -> Int
middlePage us = us !! n
  where
    n = length us `div` 2

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right (rs, us)) = parseInput $ init input
  let correct = filter (isSorted rs) us
  print . sum $ middlePage <$> correct

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let (Right (rs, us)) = parseInput $ init input
  let incorrect = filter (not . isSorted rs) us
  print . sum $ middlePage . sortByRules rs <$> incorrect

