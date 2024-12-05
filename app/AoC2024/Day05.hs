module AoC2024.Day05 (partOne, partTwo) where

import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)

-- Types
type Order = (Int, Int)
type Update = [Int]
type PrinterProgram = ([Order], [Update])

-- Parsing
number :: Parser Int
number = read <$> many1 digit

ordering :: Parser Order
ordering = do
  x <- number
  _ <- char '|'
  y <- number
  return (x, y)

updates :: Parser Update
updates = number `sepBy` char ','

printerProgram :: Parser PrinterProgram
printerProgram = do
  os <- try ordering `endBy` endOfLine
  _ <- newline
  us <- updates `sepBy` newline
  return (os, us)

parseInput :: String -> Either String PrinterProgram
parseInput s = case parse printerProgram "printer" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right (os, us)) = parseInput input
  print os
  print us

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

