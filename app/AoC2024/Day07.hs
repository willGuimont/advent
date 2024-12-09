module AoC2024.Day07 (partOne, partTwo) where

import Text.ParserCombinators.Parsec

-- Types
data Equation = Equation Int [Int] deriving (Eq, Show)

-- Parsing
number :: Parser Int
number = read <$> many1 digit

equation :: Parser Equation
equation = do
  test <- number
  _ <- string ": "
  ns <- number `sepBy` char ' '
  return $ Equation test ns

equations :: Parser [Equation]
equations = equation `sepBy` newline

parseInput :: String -> Either String [Equation]
parseInput s = case parse equations "equations" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
cartesianPower :: [a] -> Int -> [[a]]
cartesianPower _ 0 = [[]]
cartesianPower xs n = [x : ys | x <- xs, ys <- cartesianPower xs (n - 1)]

applyEq :: [Int -> Int -> Int] -> [Int] -> Int
applyEq _ [] = 0
applyEq _ [n] = n
applyEq (op : ops) (x : y : xs) = applyEq ops (x `op` y : xs)
applyEq _ (x : _) = x

isValid :: [Int -> Int -> Int] -> Equation -> Bool
isValid ops (Equation test ns) = any (\ops -> test == applyEq ops ns) opss
  where
    n = length ns - 1
    opss :: [[Int -> Int -> Int]]
    opss = cartesianPower ops n

getTest :: Equation -> Int
getTest (Equation test _) = test

concatOp :: Int -> Int -> Int
concatOp x y = read $ show x <> show y

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right eqs) = parseInput $ init input
  let result = sum $ getTest <$> filter (isValid [(+), (*)]) eqs
  print result

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let (Right eqs) = parseInput $ init input
  let result = sum $ getTest <$> filter (isValid [(+), (*), concatOp]) eqs
  print result
