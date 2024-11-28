{-# LANGUAGE TemplateHaskell #-}

module AoC2022.Day02 (main) where

import Control.Lens
import Control.Monad.Except
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec

-- Types
data Move
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq)

data Outcome
  = Win
  | Lose
  | Draw
  deriving (Show)

data Round = Round
  { _enemyMove :: Move,
    _playerMove :: Move
  }
  deriving (Show)

makeLenses ''Round

newtype Tournament = Tournament [Round] deriving (Show)

-- Parsing
parseARock :: Parser Move
parseARock = string "A" >> return Rock

parseBPaper :: Parser Move
parseBPaper = string "B" >> return Paper

parseCScissors :: Parser Move
parseCScissors = string "C" >> return Scissors

parseEnemyMove :: Parser Move
parseEnemyMove = parseARock <|> parseBPaper <|> parseCScissors

parseXRock :: Parser Move
parseXRock = string "X" >> return Rock

parseYPaper :: Parser Move
parseYPaper = string "Y" >> return Paper

parseZScissors :: Parser Move
parseZScissors = string "Z" >> return Scissors

parsePlayerMove :: Parser Move
parsePlayerMove = parseXRock <|> parseYPaper <|> parseZScissors

parseXLose :: Parser Outcome
parseXLose = string "X" >> return Lose

parseYDraw :: Parser Outcome
parseYDraw = string "Y" >> return Draw

parseZWin :: Parser Outcome
parseZWin = string "Z" >> return Win

parseOutcome :: Parser Outcome
parseOutcome = parseXLose <|> parseYDraw <|> parseZWin

parseRound :: Parser Round
parseRound = do
  enemy <- parseEnemyMove
  _ <- space
  Round enemy <$> parsePlayerMove

getMoveFromOutcome :: Move -> Outcome -> Move
getMoveFromOutcome enemy Draw = enemy
getMoveFromOutcome Rock Win = Paper
getMoveFromOutcome Paper Win = Scissors
getMoveFromOutcome Scissors Win = Rock
getMoveFromOutcome Rock Lose = Scissors
getMoveFromOutcome Paper Lose = Rock
getMoveFromOutcome Scissors Lose = Paper

parseRoundOutcome :: Parser Round
parseRoundOutcome = do
  enemy <- parseEnemyMove
  _ <- space
  Round enemy . getMoveFromOutcome enemy <$> parseOutcome

parseTournament :: Parser Tournament
parseTournament = (parseRound `sepEndBy` newline) <&> Tournament

parseInput :: String -> Either String Tournament
parseInput input = case parse parseTournament "tournament" input of
  Left err -> throwError $ show err
  Right x -> return x
  
parseTournamentOutcome :: Parser Tournament
parseTournamentOutcome = (parseRoundOutcome `sepEndBy` newline) <&> Tournament

parseInputOutcome :: String -> Either String Tournament
parseInputOutcome input = case parse parseTournamentOutcome "tournament outcome" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Scoring
moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

winnerScore :: Move -> Move -> Int
winnerScore Rock Scissors = 6
winnerScore Paper Rock = 6
winnerScore Scissors Paper = 6
winnerScore p e
  | p == e = 3
  | otherwise = 0

roundScore :: Round -> Int
roundScore r = moveScore p + winnerScore p e
  where
    p = view playerMove r
    e = view enemyMove r

tournamentScore :: Tournament -> Int
tournamentScore (Tournament xs) = sum $ fmap roundScore xs

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right tournament) = parseInput input
  print $ tournamentScore tournament
  let (Right tournamentOutcome) = parseInputOutcome input
  print $ tournamentScore tournamentOutcome
