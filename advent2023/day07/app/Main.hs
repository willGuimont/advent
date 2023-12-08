{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main (main) where

import Control.Lens.Operators ((<&>))
import Control.Monad.Except
import Data.Foldable (find)
import Data.List (group, nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

-- Types
data Card
  = ReplacedCard Card
  | NumberCard Int
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord)

data Hand = Hand [Card]
  deriving (Eq)

instance Show Hand where
  show :: Hand -> String
  show (Hand [c1, c2, c3, c4, c5]) =
    [c1, c2, c3, c4, c5] >>= \c -> case c of
      Ace -> "A"
      King -> "K"
      Queen -> "Q"
      Jack -> "J"
      NumberCard 10 -> "T"
      NumberCard n -> show n
      ReplacedCard _ -> "R"
  show (Hand _) = undefined

compareCard :: Card -> Card -> Ordering
compareCard (ReplacedCard _) (ReplacedCard _) = EQ
compareCard c1 c2 = Prelude.compare c1 c2

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1@(Hand c1) h2@(Hand c2) = if c == EQ then cc else c
    where
      c = Prelude.compare (handType h1) (handType h2)
      cc = fromMaybe EQ $ find (/= EQ) (zipWith compareCard c1 c2)

type Play = (Hand, Int)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

-- Parsing
parseCard :: Parser Card
parseCard =
  anyChar >>= \c -> case c of
    'A' -> return Ace
    'K' -> return King
    'Q' -> return Queen
    'J' -> return Jack
    'T' -> return $ NumberCard 10
    _ -> return $ NumberCard (read [c] :: Int)

parseBid :: Parser Int
parseBid = many1 digit <&> read

parseHand :: Parser Hand
parseHand = do
  c1 <- parseCard
  c2 <- parseCard
  c3 <- parseCard
  c4 <- parseCard
  c5 <- parseCard
  return $ Hand [c1, c2, c3, c4, c5]

parsePlay :: Parser Play
parsePlay = do
  h <- parseHand
  _ <- char ' '
  b <- parseBid
  return (h, b)

parseGame :: Parser [Play]
parseGame = parsePlay `sepBy` char '\n'

parseInput :: String -> Parser a -> Either String a
parseInput input p = case parse p "input" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Part 1
handType :: Hand -> HandType
handType (Hand cards)
  | fiveOfAKind = FiveOfAKind
  | fourOfAKind = FourOfAKind
  | fullHouse = FullHouse
  | threeOfAKind = ThreeOfAKind
  | twoPair = TwoPair
  | onePair = OnePair
  | otherwise = HighCard
  where
    replaced = map (\c -> case c of ReplacedCard c' -> c'; _ -> c) cards
    grouped = group . sort $ replaced
    fiveOfAKind = length (nub replaced) == 1
    fourOfAKind = any (\x -> length x == 4) grouped
    fullHouse = length (nub replaced) == 2 && (length (head grouped) == 3 || length (grouped !! 1) == 3)
    threeOfAKind = any (\x -> length x == 3) grouped
    twoPair = length (nub replaced) == 3 && length (filter (\x -> length x == 2) grouped) == 2
    onePair = length (nub replaced) == 4 && any (\x -> length x == 2) grouped

score :: [Play] -> Int
score plays = sum $ zipWith (*) bids [1 .. n]
  where
    sorted = sortOn fst plays
    bids = map snd sorted
    n = length bids

-- Part 2
replaceJokers :: Hand -> [Hand]
replaceJokers (Hand cards) = Hand <$> go cards
  where
    otherCards = [Ace, King, Queen, Jack] ++ [NumberCard n | n <- reverse [2 .. 10]]
    go [] = [[]]
    go (Jack : cs) = do
      c <- otherCards
      rest <- go cs
      return $ ReplacedCard c : rest
    go (c : cs) = do
      rest <- go cs
      return $ c : rest

bestCard :: Hand -> Hand
bestCard = maximum . replaceJokers

scoreJokers :: [Play] -> Int
scoreJokers plays = sum $ zipWith (*) bids [1 .. n]
  where
    sorted = sortOn (bestCard . fst) plays
    bids = map snd sorted
    n = length bids

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right hands) = parseInput input parseGame
  print "Part 1"
  print $ score hands
  print "Part 2"
  print $ scoreJokers hands
