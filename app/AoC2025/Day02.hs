module AoC2025.Day02 (partOne, partTwo) where

import Data.String.Utils
import Text.ParserCombinators.Parsec
import Data.List.Extra (chunksOf)
import Debug.Trace (trace)

-- Types
data Range
  = Range Int Int
  deriving (Show, Eq)

-- Parsing
parseInt :: Parser Int
parseInt = read <$> many1 digit

parseRange :: Parser Range
parseRange = do
    start <- parseInt
    _ <- char '-'
    Range start <$> parseInt

parseRanges :: Parser [Range]
parseRanges = parseRange `sepBy` char ','

parseInput :: String -> Either String [Range]
parseInput s = case parse parseRanges "lists" (rstrip s) of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
isIdInvalid :: Int -> Bool
isIdInvalid n = even len && isRepeated
    where
        s = show n
        len = length s
        [s1, s2] = chunksOf (len `div` 2) s
        isRepeated = s1 == s2

expandRange :: Range -> [Int]
expandRange (Range start end) = [start..end]

isRepeatingPattern :: Eq a => [a] -> [a] -> Bool
isRepeatingPattern [] _ = True
isRepeatingPattern _ [] = False
isRepeatingPattern pattern lst = all (== pattern) xs
    where
        plen = length pattern
        xs = chunksOf plen lst

isIdInvalid2 :: Int -> Bool
isIdInvalid2 n = go [p] ps
    where
        s = show n
        (p:ps) = s
        go _ [] = False
        go pattern xs@(x:xt) = isRepeatingPattern pattern xs || go (pattern ++ [x]) xt

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "Part One"
  let (Right rs) = parseInput input
  let rs' = filter isIdInvalid (rs >>= expandRange)
  print $ sum rs'

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "Part Two"
  let (Right rs) = parseInput input
  let rs' = filter isIdInvalid2 (rs >>= expandRange)
  print $ sum rs'
