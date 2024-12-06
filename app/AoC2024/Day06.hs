module AoC2024.Day06 (partOne, partTwo) where

import Text.ParserCombinators.Parsec
import Data.List.Utils (uniq)
import Control.Parallel.Strategies (parMap, rdeepseq)

-- Types
data Tile = Empty | Obstacle deriving (Eq)
data Direction = North | South | East | West deriving (Eq, Show)
data Guard = Guard (Int, Int) Direction deriving (Eq, Show)

instance Show Tile where
  show Empty = "."
  show Obstacle = "#"

guardChars :: String
guardChars = "^v<>"

-- Parsing
emptyTile :: Parser Tile
emptyTile = oneOf ("." <> guardChars) >> return Empty

obstacleTile :: Parser Tile
obstacleTile = char '#' >> return Obstacle

tileRow :: Parser [Tile]
tileRow = many1 (emptyTile <|> obstacleTile)

tileMap :: Parser [[Tile]]
tileMap = tileRow `sepBy` newline

parseInput :: String -> Either String [[Tile]]
parseInput s = case parse tileMap "map" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:_)  !? 0 = Just x
(_:xs) !? i = xs !? (i - 1)

findCharsCoords :: [Char] -> [String] -> [(Int, Int)]
findCharsCoords cs grid =
  [ (rowIdx, colIdx) | (rowIdx, row) <- zip [0..] grid
  , (colIdx, c) <- zip [0..] row
  , c `elem` cs]

getDirection :: Char -> Direction
getDirection '^' = North
getDirection 'v' = South
getDirection '>' = East
getDirection '<' = West
getDirection _ = error "invalid"

turnRight :: Guard -> Guard
turnRight (Guard p North) = Guard p East
turnRight (Guard p East) = Guard p South
turnRight (Guard p South) = Guard p West
turnRight (Guard p West) = Guard p North

forwardPosition :: Guard -> (Int, Int)
forwardPosition (Guard (x, y) North) = (x - 1, y)
forwardPosition (Guard (x, y) South) = (x + 1, y)
forwardPosition (Guard (x, y) East) = (x, y + 1)
forwardPosition (Guard (x, y) West) = (x, y - 1)

moveForward :: Guard -> Guard
moveForward guard@(Guard _ dir) = Guard (forwardPosition guard) dir

moveGuard :: [[Tile]] -> Guard -> Guard
moveGuard ts guard = if tileFront == Just Obstacle then turnRight guard else moveForward guard
  where
    (x, y) = forwardPosition guard
    tileFront = (ts !? x) >>= (!? y)

outOfBound :: [[Tile]] -> Guard -> Bool
outOfBound ts (Guard (x, y) _) = x < 0 || x >= nx || y < 0 || y >= ny
  where
    nx = length ts
    ny = length . head $ ts

generateGrids :: Guard -> [[Tile]] -> [(Int, Int)] -> [[[Tile]]]
generateGrids (Guard pos _) grid posToChange =
    [ replaceAt2D grid (r, c) Obstacle
    | (r, c) <- posToChange
    , (r, c) /= pos
    ]

replaceAt2D :: [[Tile]] -> (Int, Int) -> Tile -> [[Tile]]
replaceAt2D grid (rowIndex, colIndex) newTile =
    [ if r == rowIndex
      then replaceAt row colIndex newTile
      else row
    | (r, row) <- zip [0..] grid
    ]

replaceAt :: [Tile] -> Int -> Tile -> [Tile]
replaceAt row colIndex newTile =
    [ if c == colIndex then newTile else tile
    | (c, tile) <- zip [0..] row
    ]

hasLooped :: [Guard] -> Guard -> Bool
hasLooped states g = g `elem` states

hasLoop :: [Guard] -> Guard -> [[Tile]] -> Bool
hasLoop gs g ts
  | hasExited = False
  | inLoop = True
  | otherwise = hasLoop (g:gs) g' ts
  where
    hasExited = outOfBound ts g
    inLoop = hasLooped gs g
    g' = moveGuard ts g

showGrid :: [[Tile]] -> String
showGrid ts = unlines $ concat <$> map (map show) ts

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right ts) = parseInput $ init input
  let pos@(x, y) = head . findCharsCoords guardChars $ lines input
  let dir = getDirection $ lines input !! x !! y
  let g = Guard pos dir
  let guards = takeWhile (not . outOfBound ts) $ iterate (moveGuard ts) g
  let moves = uniq $ (\(Guard p _) -> p) <$> guards
  print . length $ moves

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let (Right ts) = parseInput $ init input
  let pos@(x, y) = head . findCharsCoords guardChars $ lines input
  let dir = getDirection $ lines input !! x !! y
  let g = Guard pos dir
  let guards = takeWhile (not . outOfBound ts) $ iterate (moveGuard ts) g
  let posToChange = uniq $ (\(Guard p _) -> p) <$> guards

  let grids = generateGrids g ts posToChange
  let results = parMap rdeepseq (hasLoop [] g) grids
  print . length $ filter id results

