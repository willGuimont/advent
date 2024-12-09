module AoC2024.Day08 (partOne, partTwo) where

import Data.Functor ((<&>))
import Data.List.Utils (uniq)
import Data.Ratio
import Text.ParserCombinators.Parsec

-- Types
data Tile = Empty | Antenna Char deriving (Eq)

instance Show Tile where
  show Empty = "."
  show (Antenna c) = [c]

showGrid :: [[Tile]] -> String
showGrid ts = unlines $ concat <$> map (map show) ts

-- Parsing
antennaTile :: Parser Tile
antennaTile = noneOf ".\n" <&> Antenna

emptyTile :: Parser Tile
emptyTile = char '.' >> return Empty

antennaMap :: Parser [[Tile]]
antennaMap = many1 (emptyTile <|> antennaTile) `sepBy` newline

parseInput :: String -> Either String [[Tile]]
parseInput s = case parse antennaMap "antenna" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
uniqAntenna :: [[Tile]] -> [Tile]
uniqAntenna = filter (/= Empty) . uniq . concat

antennaPositions :: [[Tile]] -> Tile -> [(Int, Int)]
antennaPositions ts t = [(x, y) | (x, r) <- zip [0 ..] ts, (y, c) <- zip [0 ..] r, c == t]

dimensions :: [[Tile]] -> (Int, Int)
dimensions ts = (length ts, length $ head ts)

inBound :: [[Tile]] -> (Int, Int) -> Bool
inBound ts (x, y) = 0 <= x && x < nx && 0 <= y && y < ny
  where
    (nx, ny) = dimensions ts

integerPositionInLine :: [[Tile]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
integerPositionInLine ts p1@(x1, y1) p2@(x2, y2)
  | p1 == p2 = []
  | x1 == x2 = filter (inBound ts) [(x1, y) | y <- rangeN]
  | y1 == y2 = filter (inBound ts) [(x, y1) | x <- rangeN]
  | otherwise = filter (inBound ts) positions
  where
    maxN = uncurry max $ dimensions ts
    dx = x2 - x1
    dy = y2 - y1
    m = dy % dx
    (dx', dy') = (denominator m, numerator m)
    rangeN = [-maxN .. maxN]
    positions = [(x1 + n * dx', y1 + n * dy') | n <- rangeN]

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = [(x, y) | x <- xs, y <- ys]

power2 :: [a] -> [(a, a)]
power2 xs = cartesian xs xs

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

antinodes :: [[Tile]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes ts p1 p2 = [p | p <- pts, respectsDist p]
  where
    pts = integerPositionInLine ts p1 p2
    respectsDist p = distance p p1 == 2 * distance p p2 || 2 * distance p p1 == distance p p2

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right ts) = parseInput $ init input
  let antennas = uniqAntenna ts
  let antennaPos = antennaPositions ts <$> antennas
  let antennaPairs = power2 <$> antennaPos
  let antis = [antinodes ts p1 p2 | ps <- antennaPairs, (p1, p2) <- ps]

  print . length . uniq $ concat antis

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let (Right ts) = parseInput $ init input
  let antennas = uniqAntenna ts
  let antennaPos = antennaPositions ts <$> antennas
  let antennaPairs = power2 <$> antennaPos
  let antis = [integerPositionInLine ts p1 p2 | ps <- antennaPairs, (p1, p2) <- ps]

  print . length . uniq $ concat antis
