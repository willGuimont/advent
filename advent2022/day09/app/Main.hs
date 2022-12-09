{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Control.Monad.Except (throwError)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

-- Types
data Move
  = UpMove
  | DownMove
  | LeftMove
  | RightMove
  deriving (Show)

-- Parsing
parseMoveHelper :: String -> Move -> Parser [Move]
parseMoveHelper s m = string s >> char ' ' >> read <$> many1 digit >>= (\num -> return . replicate num $ m)

parseMoves :: Parser [Move]
parseMoves = parseMoveHelper "U" UpMove <|> parseMoveHelper "D" DownMove <|> parseMoveHelper "L" LeftMove <|> parseMoveHelper "R" RightMove

parseMovePlan :: Parser [Move]
parseMovePlan = concat <$> parseMoves `sepBy` newline

parseInput :: String -> Either String [Move]
parseInput s = case parse parseMovePlan "moves" s of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
data World = World
  { _positions :: [(Int, Int)],
    _visitedPos :: S.Set (Int, Int)
  }
  deriving (Show)

makeLenses ''World

addPositions :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPositions (x, y) (z, w) = (x + z, y + w)

subPositions :: (Int, Int) -> (Int, Int) -> (Int, Int)
subPositions (x, y) (z, w) = (x - z, y - w)

isClose :: (Int, Int) -> (Int, Int) -> Bool
isClose x y = max (abs dx) (abs dy) <= 1
  where
    (dx, dy) = subPositions y x

normalize :: (Int, Int) -> (Int, Int)
normalize (x, y) = (signum x, signum y)

moveToDelta :: Move -> (Int, Int)
moveToDelta UpMove = (0, 1)
moveToDelta DownMove = (0, -1)
moveToDelta LeftMove = (-1, 0)
moveToDelta RightMove = (1, 0)

stepHead :: Move -> World -> World
stepHead m = over (positions . ix 0) (addPositions (moveToDelta m))

updateTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
updateTail h t@(tx, ty) = if isClose h t then (tx, ty) else addPositions t delta
  where
    delta = normalize $ subPositions h t

stepTail :: World -> Int -> World
stepTail w tIdx = updateVisited . set (positions . ix tIdx) newTPos $ w
  where
    pos = w ^. positions
    hPos = pos !! (tIdx - 1)
    tPos = pos !! tIdx
    newTPos = updateTail hPos tPos
    isLastTail = tIdx == length (view positions w) - 1
    updateVisited = if isLastTail then over visitedPos (S.insert tPos . S.insert newTPos) else id

updateTails :: World -> World
updateTails w = foldl stepTail w [1 .. numTails - 1]
  where
    numTails = length $ view positions w

toSymbol :: Int -> String
toSymbol 0 = "."
toSymbol 100 = "#"
toSymbol c = show $ c - 1

printWorld :: World -> IO ()
printWorld w = do
  let grid = [[0 | _ <- [0 .. 50]] | _ <- [0 .. 20]]
  let grid' = foldl (\g (x, y) -> set (ix (- y + 10) . ix (x + 25)) 100 g) grid (view visitedPos w)
  let grid'' = fst $ foldl (\(g, i) (x, y) -> (set (ix (- y + 10) . ix (x + 25)) i g, i + 1)) (grid', 1) (view positions w)
  mapM_ (putStrLn . concatMap toSymbol) grid''

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right moves) = parseInput input
  let endState = foldl (\w m -> updateTails . stepHead m $ w) (World [(0, 0), (0, 0)] S.empty) moves
  print . S.size $ view visitedPos endState

  let endStates = scanl (\w m -> updateTails . stepHead m $ w) (World (replicate 10 (0, 0)) S.empty) moves
  mapM_ (\w -> printWorld w >> putStrLn "-----") endStates
  print . S.size $ view visitedPos $ last endStates
