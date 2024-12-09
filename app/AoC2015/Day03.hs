module AoC2015.Day03 (main) where

import Control.Monad.Except (throwError)
import Control.Monad.State qualified as S
import Data.Map qualified as M
import Text.ParserCombinators.Parsec

-- Types
data Move
  = North
  | South
  | East
  | West
  deriving (Show)

-- Parsing
parseNorth :: Parser Move
parseNorth = char '^' >> return North

parseSouth :: Parser Move
parseSouth = char 'v' >> return South

parseEast :: Parser Move
parseEast = char '<' >> return East

parseWest :: Parser Move
parseWest = char '>' >> return West

parseMove :: Parser Move
parseMove = parseNorth <|> parseSouth <|> parseEast <|> parseWest

parseMoves :: Parser [Move]
parseMoves = many1 parseMove

parseInput :: String -> Either String [Move]
parseInput input = case parse parseMoves "moves" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
getDisplacement :: Move -> (Int, Int)
getDisplacement North = (1, 0)
getDisplacement South = (-1, 0)
getDisplacement East = (0, 1)
getDisplacement West = (0, -1)

uniqueHouseVisits :: [Move] -> Int
uniqueHouseVisits moves = M.size $ M.filter (> 0) out
  where
    loop :: Move -> S.State ((Int, Int), M.Map (Int, Int) Int) ()
    loop m = do
      ((x, y), s) <- S.get

      let (dx, dy) = getDisplacement m
      let newPos = (x + dx, y + dy)
      let newMap = if newPos `M.member` s then M.adjust (+ 1) newPos s else M.insert newPos 1 s

      S.put (newPos, newMap)
    initialHouse = M.singleton (0, 0) 1
    out = snd $ S.execState (mapM_ loop moves) ((0, 0), initialHouse)

robotUniqueVisits :: [Move] -> Int
robotUniqueVisits moves = M.size $ M.filter (> 0) out
  where
    loop :: Move -> S.State (Bool, (Int, Int), (Int, Int), M.Map (Int, Int) Int) ()
    loop m = do
      (isSanta, (sx, sy), (rx, ry), s) <- S.get
      let (dx, dy) = getDisplacement m
      let (x, y) = if isSanta then (sx, sy) else (rx, ry)

      let newPos = (x + dx, y + dy)
      let newMap = if newPos `M.member` s then M.adjust (+ 1) newPos s else M.insert newPos 1 s

      S.put (not isSanta, if isSanta then newPos else (sx, sy), if not isSanta then newPos else (rx, ry), newMap)
    initialHouse = M.singleton (0, 0) 2
    (_, _, _, out) = S.execState (mapM_ loop moves) (True, (0, 0), (0, 0), initialHouse)

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right moves) = parseInput input
  print . uniqueHouseVisits $ moves
  print . robotUniqueVisits $ moves
