module AoC2023.Day02 (main) where

import Control.Monad.Except
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

-- Types
data Color
  = Red
  | Green
  | Blue
  deriving (Eq, Ord, Show)

data Draw = Draw
  { cubes :: M.Map Color Int
  }
  deriving (Show)

data Game = Game
  { gameNum :: Int,
    draws :: [Draw]
  }
  deriving (Show)

-- Parsing
parseRed :: Parser Color
parseRed = string "red" >> return Red

parseGreen :: Parser Color
parseGreen = string "green" >> return Green

parseBlue :: Parser Color
parseBlue = string "blue" >> return Blue

parseColor :: Parser Color
parseColor = parseRed <|> parseGreen <|> parseBlue

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseOneDraw :: Parser (Color, Int)
parseOneDraw = do
  _ <- many space
  i <- parseInt
  _ <- char ' '
  c <- parseColor
  return (c, i)

parseDraw :: Parser Draw
parseDraw = do
  xs <- parseOneDraw `sepBy` string ", "
  return $ Draw $ M.fromList xs

parseGame :: Parser Game
parseGame = do
  _ <- string "Game "
  i <- parseInt
  _ <- char ':'
  ds <- parseDraw `sepBy` char ';'
  return $ Game i ds

parseGames :: Parser [Game]
parseGames = parseGame `sepBy` char '\n'

parseInput :: String -> Either String [Game]
parseInput input = case parse parseGames "games" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Part 1
maxCubes :: M.Map Color Int
maxCubes = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

isDrawPossible :: Draw -> Bool
isDrawPossible d = M.foldr (&&) True $ M.mapWithKey (\k v -> v <= M.findWithDefault 0 k maxCubes) (cubes d)

isGamePossible :: Game -> Bool
isGamePossible g = and $ isDrawPossible <$> (draws g)

-- Part 2
minCubes :: Game -> M.Map Color Int
minCubes g = M.unionsWith max $ cubes <$> draws g

power :: M.Map Color Int -> Int
power = M.foldr (*) 1

-- Main
main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right games) = parseInput input
  print $ sum $ gameNum <$> filter isGamePossible games
  print $ sum $ map (power . minCubes) games
