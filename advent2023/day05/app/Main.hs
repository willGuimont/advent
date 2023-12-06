module Main (main) where

import Control.Lens ((<&>))
import Control.Monad.Except
import Data.List (sortOn)
import Data.Set qualified as S
import Text.ParserCombinators.Parsec

-- Types
type Seeds = S.Set Integer

type SeedRanges = [(Integer, Integer)]

data Entry = Entry
  { entryDestRangeStart :: Integer,
    entrySourceRangeStart :: Integer,
    entryRangeLength :: Integer
  }
  deriving (Show)

type AlmanachEntry = [Entry]

data Almanach = Almanach
  { almanachSeeds :: Seeds,
    almanachEntries :: [AlmanachEntry]
  }
  deriving (Show)

data AlmanachRange = AlmanachRange
  { almanacRangeSeedRanges :: SeedRanges,
    almanachRangeEntries :: [AlmanachEntry]
  }
  deriving (Show)

-- Parsing
integer :: Parser Integer
integer = read <$> many1 digit

nnlSpaces :: Parser ()
nnlSpaces = skipMany (oneOf " \t")

skipLine :: Parser Char
skipLine = skipMany (noneOf "\n") *> newline

parseSeeds :: Parser Seeds
parseSeeds = (string "seeds:" *> nnlSpaces *> (integer `sepBy` nnlSpaces)) <&> S.fromList

parseEntry :: Parser Entry
parseEntry =
  Entry
    <$> integer
    <*> (space *> integer)
    <*> (space *> integer)
    <* newline

parseAlmanachEntry :: Parser AlmanachEntry
parseAlmanachEntry = skipLine *> many1 parseEntry

parseAlmanach :: Parser Almanach
parseAlmanach = do
  seeds <- parseSeeds <* spaces
  entries <- parseAlmanachEntry `sepBy1` spaces
  return $ Almanach seeds entries

parseSeedRange :: Parser (Integer, Integer)
parseSeedRange = (,) <$> (integer <* space) <*> integer

parseSeedRanges :: Parser SeedRanges
parseSeedRanges = string "seeds:" *> nnlSpaces *> parseSeedRange `sepBy` nnlSpaces

parseAlmanachRange :: Parser AlmanachRange
parseAlmanachRange = do
  seeds <- parseSeedRanges <* spaces
  entries <- parseAlmanachEntry `sepBy1` spaces
  return $ AlmanachRange seeds entries

parseInput :: String -> Parser a -> Either String a
parseInput input p = case parse p "seeds" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Part 1
inEntryRange :: Entry -> Integer -> Bool
inEntryRange (Entry _ sourceStart rangeLen) seed = seed >= sourceStart && seed < sourceStart + rangeLen

applyEntries :: AlmanachEntry -> Integer -> Integer
applyEntries [] seed = seed
applyEntries (e@(Entry destStart sourceStart _) : es) seed
  | inEntryRange e seed = seed - sourceStart + destStart
  | otherwise = applyEntries es seed

getLocations :: Almanach -> Integer
getLocations (Almanach seeds []) = minimum seeds
getLocations (Almanach seeds (e : es)) = getLocations (Almanach nSeeds es)
  where
    nSeeds = S.map (applyEntries e) seeds

-- Part 2
rangeIntersection :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
rangeIntersection (s1, l1) (s2, l2)
  | s1 + l1 < s2 || s2 + l2 < s1 = (0, 0)
  | otherwise = (max s1 s2, min (s1 + l1) (s2 + l2) - max s1 s2)

rangeDifference :: (Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)]
rangeDifference (s1, l1) (s2, l2)
  | s1 >= s2 + l2 || s2 >= s1 + l1 = [(s1, l1)] -- No overlap, return the original range
  | s1 < s2 && s1 + l1 > s2 + l2 = [(s1, s2 - s1), (s2 + l2, s1 + l1 - (s2 + l2))] -- Split into two ranges
  | s1 < s2 = [(s1, s2 - s1)] -- Overlaps on the left
  | s1 + l1 > s2 + l2 = [(s2 + l2, s1 + l1 - (s2 + l2))] -- Overlaps on the right
  | otherwise = [] -- Complete overlap, return an empty list

rangeUnion :: (Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)]
rangeUnion (s1, l1) (s2, l2)
  | s1 + l1 < s2 || s2 + l2 < s1 = [(s1, l1), (s2, l2)] -- No overlap, return both ranges
  | otherwise = [(min s1 s2, max (s1 + l1) (s2 + l2) - min s1 s2)] -- Overlap, return the union

rangeOverlap :: (Integer, Integer) -> (Integer, Integer) -> Bool
rangeOverlap (s1, l1) (s2, l2) = s1 + l1 - 1 >= s2 && s2 + l2 - 1 >= s1

overlapEntry :: Entry -> (Integer, Integer) -> Bool
overlapEntry (Entry _ sourceStart rangeLen) (s, l) = rangeOverlap (sourceStart, rangeLen) (s, l)

applyEntryRange :: Entry -> (Integer, Integer) -> (Integer, Integer)
applyEntryRange (Entry destStart sourceStart rangeLen) (s, l)
  | rangeOverlap (sourceStart, rangeLen) (s, l) =
      if s < sourceStart
        then undefined
        else (destStart + s - sourceStart, l)
  | otherwise = (s, l)

splitRangeForEntry :: Entry -> (Integer, Integer) -> [(Integer, Integer)]
splitRangeForEntry (Entry _ sourceStart rangeLen) r = filter ((> 0) . snd) $ rangeIntersection r eRange : rangeDifference r eRange
  where
    eRange = (sourceStart, rangeLen)

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges (a@(s1, l1) : b@(s2, l2) : rs)
  | rangeOverlap a b = mergeRanges (rangeUnion a b <> rs)
  | otherwise = (s1, l1) : mergeRanges ((s2, l2) : rs)

housekeeping :: [(Integer, Integer)] -> [(Integer, Integer)]
housekeeping = mergeRanges . sortOn fst . filter ((> 0) . snd)

applyEntriesRange :: AlmanachEntry -> [(Integer, Integer)] -> [(Integer, Integer)]
applyEntriesRange [] range = range
applyEntriesRange (e : es) range = (applyEntryRange e <$> overlapping) <> applyEntriesRange es notOverlapping
  where
    eRange = (entrySourceRangeStart e, entryRangeLength e)
    overlapping = filter ((> 0) . snd) $ rangeIntersection eRange <$> range
    notOverlapping = concat $ rangeDifference <$> range <*> pure eRange

getLocationsRange :: AlmanachRange -> Integer
getLocationsRange (AlmanachRange seeds []) = minimum $ map fst seeds
getLocationsRange (AlmanachRange seeds (e : es)) = getLocationsRange (AlmanachRange nSeeds es)
  where
    nSeeds = housekeeping $ applyEntriesRange e $ housekeeping seeds

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  --   input <- readFile "data/sample.txt"
  let (Right almanach) = parseInput input parseAlmanach
  print "Part 1"
  print $ getLocations almanach
  let (Right almanachRange) = parseInput input parseAlmanachRange
  print "Part 2"
  print $ getLocationsRange almanachRange
