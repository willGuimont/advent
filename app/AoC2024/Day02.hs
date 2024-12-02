module AoC2024.Day02 (partOne, partTwo) where

import Text.ParserCombinators.Parsec

-- Parsing
parseLevel :: Parser Int
parseLevel = read <$> many1 digit

parseReport :: Parser [Int]
parseReport = parseLevel `sepBy` char ' '

parseReports :: Parser [[Int]]
parseReports = parseReport `sepBy` char '\n'

parseInput :: String -> Either String [[Int]]
parseInput s = case parse parseReports "reports" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

countTrue :: [Bool] -> Int
countTrue = sum . fmap boolToInt

computeDelta :: [Int] -> [Int]
computeDelta x = zipWith (-) x (tail x)

isSafe :: [Int] -> Bool
isSafe xs = all (\x -> 1 <= x && x <= 3) delta || all (\x -> -3 <= x && x <= -1) delta
  where
    delta = computeDelta xs

removeOne :: [a] -> [[a]]
removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "Part One"

  let (Right xs) = parseInput $ init input
  print . countTrue $ isSafe <$> xs


partTwo :: String -> IO ()
partTwo input = do
  putStrLn "Part Two"

  let (Right xs) = parseInput $ init input
  let safeReports = isSafe <$> xs
  let subReports = removeOne <$> xs
  let safeSubReports = any isSafe <$> subReports
  let result =  zipWith (||) safeReports safeSubReports
  print . countTrue $ result

