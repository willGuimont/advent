module AoC2022.Day13 (main) where

import Control.Monad.Except (throwError)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

-- Types
data Packet
  = NumberPacket Int
  | ListPacket [Packet]
  deriving (Show, Eq)

-- Parsing
parseNumberPacket :: Parser Packet
parseNumberPacket = NumberPacket . read <$> many1 digit

parseListPacket :: Parser Packet
parseListPacket = ListPacket <$> (char '[' *> parsePacket `sepBy` char ',' <* char ']')

parsePacket :: Parser Packet
parsePacket = parseNumberPacket <|> parseListPacket

parsePacketPair :: Parser (Packet, Packet)
parsePacketPair = do
  p1 <- parsePacket
  _ <- newline
  p2 <- parsePacket
  _ <- newline
  return (p1, p2)

parseInput :: String -> Either String [(Packet, Packet)]
parseInput s = case parse (parsePacketPair `sepBy` newline) "packets" s of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
compareList :: Ord a => [a] -> [a] -> Ordering
compareList [] [] = EQ
compareList [] (_ : _) = LT
compareList (_ : _) [] = GT
compareList (x : xs) (y : ys) = let cmp = compare x y in (if cmp == EQ then compareList xs ys else cmp)

instance Ord Packet where
  compare (NumberPacket x) (NumberPacket y) = compare x y
  compare (ListPacket x) (ListPacket y) = compareList x y
  compare x@(NumberPacket _) y@(ListPacket _) = compare (ListPacket [x]) y
  compare x@(ListPacket _) y@(NumberPacket _) = compare x (ListPacket [y])

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right packetPairs) = parseInput input
      dividers = [ListPacket [ListPacket [NumberPacket 2]], ListPacket [ListPacket [NumberPacket 6]]]
      allPackets = sort $ concatMap (\(x, y) -> [x, y]) packetPairs <> dividers
      dividerIndices = (+ 1) . fromJust . (`elemIndex` allPackets) <$> dividers
  print . sum . fmap fst . filter snd . zip [1 ..] $ uncurry (<) <$> packetPairs
  print . product $ dividerIndices
