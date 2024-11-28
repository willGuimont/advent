module AoC2023.Day06 (main) where

import Control.Monad.Except
import Data.Functor ((<&>))
import Debug.Trace (trace)
import Text.ParserCombinators.Parsec

-- Types
type Time = Int

type Distance = Int

type Race = (Time, Distance)

-- Parsing
integer :: Parser Int
integer = read <$> many1 digit

integerStr :: Parser String
integerStr = many1 digit

nnlSpaces :: Parser ()
nnlSpaces = skipMany $ oneOf " \t"

parseTimes :: Parser [Time]
parseTimes = string "Time:" *> spaces *> integer `sepBy` nnlSpaces

parseDistances :: Parser [Distance]
parseDistances = string "Distance:" *> spaces *> integer `sepBy` nnlSpaces

parseRaces :: Parser [Race]
parseRaces = do
  times <- parseTimes
  _ <- newline
  zip times <$> parseDistances

parseKerningTime :: Parser Time
parseKerningTime = (string "Time:" *> spaces *> integerStr `sepBy` nnlSpaces) <&> read . concat

parseKerningDistance :: Parser Distance
parseKerningDistance = (string "Distance:" *> spaces *> integerStr `sepBy` nnlSpaces) <&> read . concat

parseKerningRace :: Parser Race
parseKerningRace = do
  times <- parseKerningTime
  _ <- newline
  distances <- parseKerningDistance
  return (times, distances)

parseInput :: String -> Parser a -> Either String a
parseInput input p = case parse p "input" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
quadraticSolver :: Double -> Double -> Double -> [Double]
quadraticSolver a b c
  | discriminant < 0 = []
  | discriminant == 0 = [(-b) / (2 * a)]
  | otherwise = [(-b + sqrt discriminant) / (2 * a), (-b - sqrt discriminant) / (2 * a)]
  where
    discriminant = b * b - 4 * a * c

solveRace :: Time -> Distance -> [Double]
solveRace time distance = quadraticSolver (-1) (fromIntegral time) (negate $ fromIntegral distance)

numWins :: Int -> Int -> Int
numWins time distance = f $ solveRace time distance
  where
    f [] = 0
    f [_] = 1
    f [x, y] = z1 - z2 + 1 - d1 - d2
      where
        z1 = floor (max x y)
        z2 = ceiling (min x y)
        sol1 = -z1 * z1 + time * z1 - distance
        sol2 = -z2 * z2 + time * z2 - distance
        d1 = if sol1 == 0 then 1 else 0
        d2 = if sol2 == 0 then 1 else 0
    f _ = error "Invalid number of solutions"

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right races) = parseInput input parseRaces
  print "Part 1"
  print . product $ fmap (uncurry numWins) races
  let (Right kerningRace) = parseInput input parseKerningRace
  print "Part 2"
  print $ uncurry numWins kerningRace
