module Main (main) where

import Control.Monad.Except
import Data.Map qualified as M
import DefaultMap qualified as DM
import Text.ParserCombinators.Parsec

-- Types
data Card = Card
  { cardNumber :: Int,
    winningNumbers :: [Int],
    drawnNumbers :: [Int]
  }
  deriving (Show)

-- Parsing
integer :: Parser Int
integer = read <$> many1 digit

nonNewlineSpaces :: Parser ()
nonNewlineSpaces = skipMany (oneOf " \t")

parseNumbers :: Parser [Int]
parseNumbers = many1 (integer <* nonNewlineSpaces)

parseCard :: Parser Card
parseCard = do
  _ <- string "Card" >> spaces
  num <- integer
  _ <- char ':' >> spaces
  winning <- parseNumbers
  _ <- char '|' >> spaces
  Card num winning <$> parseNumbers

parseCards :: Parser [Card]
parseCards = parseCard `sepBy` char '\n'

parseInput :: String -> Either String [Card]
parseInput input = case parse parseCards "cards" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Part 1
getNumWinningNumbers :: Card -> Int
getNumWinningNumbers card = length $ filter (`elem` winningNumbers card) (drawnNumbers card)

getScore :: Int -> Int
getScore 0 = 0
getScore 1 = 1
getScore n = 2 * getScore (n - 1)

-- Part 2
getNumWinningMap :: [Card] -> M.Map Int Int
getNumWinningMap cards = M.fromList $ zip [1 ..] (getNumWinningNumbers <$> cards)

duplicateCards :: Int -> M.Map Int Int -> DM.DefaultMap Int Int -> DM.DefaultMap Int Int
duplicateCards ci wm nm
  | ci == M.size wm + 1 = nm
  | otherwise = duplicateCards (ci + 1) wm newNm
  where
    numWin = M.findWithDefault 0 ci wm
    numCard = DM.lookup ci nm
    cardsToDup = [c + ci | c <- [1 .. numWin]]
    newNm = foldr (DM.adjust (+ numCard)) nm cardsToDup

getScore2 :: Int -> M.Map Int Int -> DM.DefaultMap Int Int -> Int -> Int
getScore2 ci wm nm acc
  | ci == M.size wm + 1 = acc
  | otherwise = getScore2 (ci + 1) wm nm (acc + numCards)
  where
    numCards = DM.lookup ci nm

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  cards <- either (error . show) return (parseInput input)
  print . sum $ getScore . getNumWinningNumbers <$> cards
  let winMap = getNumWinningMap cards
  let numCards = DM.empty 1 :: DM.DefaultMap Int Int
  let dupCards = duplicateCards 1 winMap numCards
  print $ getScore2 1 winMap dupCards 0
