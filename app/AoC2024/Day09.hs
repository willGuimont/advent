module AoC2024.Day09 (partOne, partTwo) where

import Data.Char (chr, ord)
import Data.List (singleton)
import Data.Maybe (isNothing)
import Safe (headMay)
import Text.ParserCombinators.Parsec

-- Types
data DiskMapContent = Free Int | File Int Int deriving (Eq)

type DiskMap = [DiskMapContent]

instance Show DiskMapContent where
  show (Free x) = replicate x '.'
  -- show (File i x) = replicate x (intToDigit i)
  show (File i x) = replicate x (chr $ i + ord 'a')

-- Parsing
number :: Parser Int
number = read . singleton <$> digit

diskMap :: Parser [Int]
diskMap = many1 number

parseInput :: String -> Either String [Int]
parseInput s = case parse diskMap "disk" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
mkDiskMap :: [Int] -> DiskMap
mkDiskMap = mkFile 0
  where
    mkFile _ [] = []
    mkFile i (x : xs) = File i x : mkFree (i + 1) xs

    mkFree _ [] = []
    mkFree i (x : xs) = Free x : mkFile i xs

isFree :: DiskMapContent -> Bool
isFree (Free _) = True
isFree _ = False

isFile :: DiskMapContent -> Bool
isFile (File _ _) = True
isFile _ = False

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx newList targetList =
  let (before, _ : after) = splitAt idx targetList
   in before <> [newList] <> after

replaceAts :: Int -> [a] -> [a] -> [a]
replaceAts idx newList targetList =
  let (before, _ : after) = splitAt idx targetList
   in before <> newList <> after

removeAt :: Int -> [a] -> [a]
removeAt idx xs = take idx xs ++ drop (idx + 1) xs

defragmentPerBlock :: DiskMap -> DiskMap
defragmentPerBlock [] = []
defragmentPerBlock ((Free n) : ds)
  | null files = ds
  | fn < n = File i fn : defragmentPerBlock (Free (n - fn) : ds' <> [Free fn])
  | fn == n = File i n : defragmentPerBlock (ds' <> [Free n])
  | fn > n = File i n : defragmentPerBlock (ds' <> [File i (fn - n), Free n])
  where
    files = filter (isFile . snd) $ zip [0 ..] ds
    (fIdx, File i fn) = last files
    ds' = removeAt fIdx ds
defragmentPerBlock (d : ds) = d : defragmentPerBlock ds

toBlocks :: DiskMap -> DiskMap
toBlocks [] = []
toBlocks (File i n : ds) = replicate n (File i 1) <> toBlocks ds
toBlocks (Free n : ds) = replicate n (Free 1) <> toBlocks ds

defragmentPerFile :: DiskMap -> DiskMap
defragmentPerFile = reverse . defragmentPerFileHelper . reverse

fuseFrees :: DiskMap -> DiskMap
fuseFrees [] = []
fuseFrees (Free n1 : Free n2 : ds) = fuseFrees $ Free (n1 + n2) : ds
fuseFrees ds = ds

defragmentPerFileHelper :: DiskMap -> DiskMap
defragmentPerFileHelper [] = []
defragmentPerFileHelper (f@(File _ fn) : ds)
  | isNothing freeMay = fuseFrees $ f : defragmentPerFileHelper ds
  | fn == n = fuseFrees $ Free fn : defragmentPerFileHelper ds'
  | fn < n = fuseFrees $ Free fn : defragmentPerFileHelper ds''
  where
    frees = reverse . filter (\(_, Free q) -> q >= fn) . filter (isFree . snd) $ zip [0 ..] ds
    freeMay = headMay frees
    Just (fIdx, Free n) = freeMay
    ds' = replaceAt fIdx f ds
    ds'' = replaceAts fIdx [Free (n - fn), f] ds
defragmentPerFileHelper (d : ds) = d : defragmentPerFileHelper ds

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right xs) = parseInput $ init input
  let dm = mkDiskMap xs
  let dm' = defragmentPerBlock dm
  let result = sum $ fmap (\(x, File i 1) -> i * x) $ filter (isFile . snd) $ zip [0 ..] $ toBlocks dm'
  print result

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let (Right xs) = parseInput $ init input
  let dm = mkDiskMap xs
  let dm' = defragmentPerFile dm
  let result = sum $ fmap (\(x, File i 1) -> i * x) $ filter (isFile . snd) $ zip [0 ..] $ toBlocks dm'
  print result
