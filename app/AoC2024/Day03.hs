module AoC2024.Day03 (partOne, partTwo) where

import Text.ParserCombinators.Parsec

-- Types
data Expression
  = Mul Int Int
  | Invalid
  | Do
  | Dont
  deriving (Show, Eq)

type Program = [Expression]

-- Parsing
number :: Parser Int
number = read <$> many1 digit

parseExpression :: Parser Expression
parseExpression = do
  _ <- string "mul("
  x <- number
  _ <- char ','
  y <- number
  _ <- char ')'
  pure $ Mul x y

parseDo :: Parser Expression
parseDo = string "do()" >> pure Do

parseDont :: Parser Expression
parseDont = string "don't()" >> pure Dont

ignoreRest :: Parser Expression
ignoreRest = anyChar >> pure Invalid

parseProgram :: Parser Program
parseProgram = many (try parseExpression <|> try parseDo <|> try parseDont <|> ignoreRest)

parseInput :: String -> Either String Program
parseInput s = case parse parseProgram "program" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
interpretExpression :: Expression -> Int
interpretExpression (Mul x y) = x * y
interpretExpression _ = 0

applyDoDont :: Program -> Program
applyDoDont = loop [] True . filter (/= Invalid)
  where
    loop c _ [] = c
    loop c _ (Dont : xs) = loop c False xs
    loop c _ (Do : xs) = loop c True xs
    loop c e (x : xs) = loop c' e xs
      where
        c' = if e then x : c else c

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right xs) = parseInput $ init input
  let result = sum $ interpretExpression <$> xs
  print result

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let (Right xs) = parseInput $ init input
  let result = sum $ interpretExpression <$> applyDoDont xs
  print result
