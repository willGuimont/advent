module AoC2024.Day02 (partOne, partTwo) where

import Text.ParserCombinators.Parsec
import Data.List (zipWith3)

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

allButOne :: [Bool] -> Bool
allButOne bs = n - 1 == sum (boolToInt <$> bs)
  where
    n = length bs

safeButOne :: [Bool] -> [Bool] -> [Bool] -> Bool
safeButOne small pos neg = allButOne pos' || allButOne neg'
  where
    pos' = zipWith (&&) small pos
    neg' = zipWith (&&) small neg
    

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "Part One"

  let (Right xs) = parseInput $ init input
  let delta = (\x -> zipWith (-) x (tail x)) <$> xs
  let positive = all (> 0) <$> delta
  let negative = all (< 0) <$> delta
  let smallChange = all ((<= 3) . abs) <$> delta
  let safe = zipWith3 (\x y z -> x && (y || z)) smallChange positive negative
  print . sum $ boolToInt <$> safe
    

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "Part Two"

  let (Right xs) = parseInput $ init input
  let delta = (\x -> zipWith (-) x (tail x)) <$> xs
  let positive = fmap (> 0) <$> delta
  let negative = fmap (< 0) <$> delta
  let smallChange = fmap ((<= 3) . abs) <$> delta
  let safeDampened = zipWith3 safeButOne smallChange positive negative
  print safeDampened
  print . sum $ boolToInt <$> safeDampened

