{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Control.Monad.Except
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec

-- Types
newtype Crate = Crate Char deriving (Show)

newtype Move = Move (Int, Int, Int) deriving (Show)

data SupplyPlan = SupplyPlan
  { _crates :: [[Crate]],
    _moves :: [Move]
  }
  deriving (Show)

makeLenses ''SupplyPlan

-- Parsing
trySepBy :: Parser a -> Parser sep -> Parser [a]
trySepBy p sep = do
  r <- optionMaybe p
  case r of
    Nothing -> return []
    Just x -> (x :) <$> many (try $ sep >> p)

parseCrate :: Parser (Maybe Crate)
parseCrate = char '[' *> (Just . Crate <$> letter) <* char ']'

parseEmpty :: Parser (Maybe Crate)
parseEmpty = string "   " >> return Nothing

parseCratesByRow :: Parser [[Maybe Crate]]
parseCratesByRow = ((parseCrate <|> parseEmpty) `sepBy` char ' ') `trySepBy` char '\n'

toCratesByCol :: [[Maybe Crate]] -> [[Crate]]
toCratesByCol xs = catMaybes <$> transpose xs

parseColNum :: Parser [Int]
parseColNum = spaces *> (read <$> many1 digit) `trySepBy` many1 (char ' ') <* optional (char ' ') <* newline

parseMove :: Parser Move
parseMove = do
  _ <- string "move "
  num <- read <$> many1 digit
  _ <- string " from "
  fromIdx <- read <$> many1 digit
  _ <- string " to "
  toIdx <- read <$> many1 digit
  return $ Move (num, fromIdx - 1, toIdx - 1)

parseMoves :: Parser [Move]
parseMoves = parseMove `sepBy` newline

parseSupplyPlan :: Parser SupplyPlan
parseSupplyPlan = do
  byRow <- parseCratesByRow
  _ <- parseColNum
  _ <- newline
  crateMoves <- parseMoves

  let cratesByCols = toCratesByCol byRow
  return $ SupplyPlan cratesByCols crateMoves

parseInput :: String -> Either String SupplyPlan
parseInput input = case parse parseSupplyPlan "crates" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
moveOneCrate :: Int -> Int -> [[Crate]] -> [[Crate]]
moveOneCrate fromIdx toIdx cs = (over (ix toIdx) (movedCrate :) . over (ix fromIdx) (drop 1)) cs
  where
    movedCrate = head $ view (ix fromIdx) cs

applyMove9000 :: SupplyPlan -> Move -> SupplyPlan
applyMove9000 plan (Move (0, _, _)) = plan
applyMove9000 plan (Move (num, fromIdx, toIdx)) = applyMove9000 (over crates (moveOneCrate fromIdx toIdx) plan) (Move (num - 1, fromIdx, toIdx))

moveCrates :: Move -> [[Crate]] -> [[Crate]]
moveCrates (Move (num, fromIdx, toIdx)) cs = (over (ix toIdx) (movedCrates ++) . over (ix fromIdx) (drop num)) cs
  where
    movedCrates = take num $ view (ix fromIdx) cs

applyMove9001 :: SupplyPlan -> Move -> SupplyPlan
applyMove9001 plan move = over crates (moveCrates move) plan

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  print $ parseInput input
  let (Right plan) = parseInput input

  let endState9000 = foldl applyMove9000 plan (view moves plan)
  let endState9001 = foldl applyMove9001 plan (view moves plan)

  print $ view crates plan
  print $ view crates endState9000
  putStrLn $ (\(Crate c) -> c) . head <$> view crates endState9000
  putStrLn $ (\(Crate c) -> c) . head <$> view crates endState9001
