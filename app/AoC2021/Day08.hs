{-# LANGUAGE MultiWayIf #-}

module AoC2021.Day08 where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import Data.Set qualified as Set

-- Types
data Segment = A | B | C | D | E | F | G deriving (Show, Read, Eq, Ord)

type Config = (Segment, Segment, Segment, Segment, Segment, Segment, Segment)

-- Execution
parseLine :: String -> ([String], [String])
parseLine s = let [patterns, output] = map words $ splitOn "|" s in (patterns, output)

parseAll :: String -> [([String], [String])]
parseAll = map parseLine . lines

part1 :: ([String], [String]) -> Int
part1 (_, outputs) = sum . map (go . length) $ outputs
  where
    go 2 = 1 -- one
    go 4 = 1 -- four
    go 3 = 1 -- seven
    go 7 = 1 -- eight
    go _ = 0 -- otherwise

toSegment :: Char -> Segment
toSegment c = read [toUpper c]

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

solve :: [Set.Set Segment] -> Config
solve patterns = (a, b, c, d, e, f, g)
  where
    getOfSize s = filter ((== s) . Set.size)

    withoutLetters :: [Segment] -> [Set.Set Segment] -> [Set.Set Segment]
    withoutLetters xs = map (`Set.difference` Set.fromList xs)

    getSingleton = head . Set.toList

    findLetters s exclude ps = head $ getOfSize s $ withoutLetters exclude ps
    findLetterByUnique exclude ps = getSingleton $ findLetters 1 exclude ps

    one = head $ getOfSize 2 patterns
    four = head $ getOfSize 4 patterns
    seven = head $ getOfSize 3 patterns
    eight = head $ getOfSize 7 patterns
    a = getSingleton $ seven `Set.difference` one
    g = findLetterByUnique (Set.toList four <> [a]) patterns
    d = findLetterByUnique (Set.toList one <> [a, g]) patterns
    be = findLetters 2 (Set.toList one <> [a, g, d]) patterns
    f = findLetterByUnique (Set.toList be) $ getOfSize 3 (withoutLetters [a, g, d] patterns)
    c = getSingleton $ one `Set.difference` Set.fromList [f]
    b = getSingleton $ four `Set.difference` Set.fromList [c, d, f]
    e = getSingleton $ eight `Set.difference` Set.fromList [a, b, c, d, f, g]

toDigit :: Config -> Set.Set Segment -> Int
toDigit (a, b, c, d, e, f, g) s =
  if
    | s == zero -> 0
    | s == one -> 1
    | s == two -> 2
    | s == three -> 3
    | s == four -> 4
    | s == five -> 5
    | s == six -> 6
    | s == seven -> 7
    | s == eight -> 8
    | s == nine -> 9
    | otherwise -> 0
  where
    zero = Set.fromList [a, b, c, e, f, g]
    one = Set.fromList [c, f]
    two = Set.fromList [a, c, d, e, g]
    three = Set.fromList [a, c, d, f, g]
    four = Set.fromList [b, c, d, f]
    five = Set.fromList [a, b, d, f, g]
    six = Set.fromList [a, b, d, e, f, g]
    seven = Set.fromList [a, c, f]
    eight = Set.fromList [a, b, c, d, e, f, g]
    nine = Set.fromList [a, b, c, d, f, g]

getOutput :: ([String], [String]) -> Int
getOutput line = sum $ zipWith (\x i -> x * 10 ^ i) (reverse digits) [0 ..]
  where
    (patterns, outputs) = mapTuple (map (Set.fromList . map toSegment)) line
    cfg = solve patterns
    digits = map (toDigit cfg) outputs

-- Main
main :: IO ()
main = do
  file <- readFile "input.txt"
  let parsed = parseAll file
      result1 = sum $ map part1 parsed
   in do
        print result1
        print $ sum $ map getOutput parsed
