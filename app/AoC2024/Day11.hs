module AoC2024.Day11 (partOne, partTwo) where

import Data.HashMap.Strict (HashMap, elems, empty, insertWith, mapWithKey, union)
import Text.ParserCombinators.Parsec

-- Parsing
number :: Parser (Int, Int)
number = many1 digit >>= \x -> return (read x, 1)

parseStones :: Parser [(Int, Int)]
parseStones = number `sepBy` spaces

parseInput :: String -> Either String [(Int, Int)]
parseInput s = case parse parseStones "stones" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
toStones :: [(Int, Int)] -> HashMap Int Int
toStones = go empty
  where
    go hm [] = hm
    go hm ((sn, n) : xs) = go hm' xs
      where
        hm' = insertWith (+) sn n hm

splitString :: String -> (String, String)
splitString str =
  let len = length str
      half = len `div` 2
   in splitAt half str

blinkStone :: Int -> Int -> [(Int, Int)]
blinkStone stoneNum num
  | stoneNum == 0 = [(1, num)]
  | even (length stoneNumStr) = [(beginNum, num), (endNum, num)]
  | otherwise = [(stoneNum * 2024, num)]
  where
    stoneNumStr = show stoneNum
    (begin, end) = splitString stoneNumStr
    (beginNum, endNum) = (read begin, read end)

blink :: HashMap Int Int -> HashMap Int Int
blink = toStones . concat . elems . mapWithKey blinkStone

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right xs) = parseInput $ init input
  let stones = toStones xs
  let stones' = nTimes 25 blink stones
  let result = sum $ elems stones'
  print result

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let (Right xs) = parseInput $ init input
  let stones = toStones xs
  let stones' = nTimes 75 blink stones
  let result = sum $ elems stones'
  print result
