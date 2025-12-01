module AoC2025.Day01 (partOne, partTwo) where

import Data.String.Utils
import Text.ParserCombinators.Parsec

-- Types
data Rotation
  = LeftRotation Int
  | RightRotation Int
  deriving (Show, Eq)

-- Parsing
parseInt :: Parser Int
parseInt = read <$> many1 digit

parseLeft :: Parser Rotation
parseLeft = LeftRotation <$> (char 'L' >> parseInt)

parseRight :: Parser Rotation
parseRight = RightRotation <$> (char 'R' >> parseInt)

parseRotation :: Parser Rotation
parseRotation = parseLeft <|> parseRight

parseRotations :: Parser [Rotation]
parseRotations = parseRotation `sepBy` char '\n'

parseInput :: String -> Either String [Rotation]
parseInput s = case parse parseRotations "lists" (rstrip s) of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
rotationToOffset :: Rotation -> Int
rotationToOffset (LeftRotation x) = -x
rotationToOffset (RightRotation x) = x

applyRotation :: [Int] -> [Rotation] -> [Int]
applyRotation [] _ = error "applyRotation: empty current position"
applyRotation cs [] = cs
applyRotation cs@(c : _) (r : rs) =
  let p = ((c + rotationToOffset r) `mod` 100)
   in applyRotation (p : cs) rs

zeroToOne :: Int -> Int
zeroToOne 0 = 1
zeroToOne _ = 0

splitTicks :: Rotation -> [Rotation]
splitTicks (LeftRotation x) = replicate x (LeftRotation 1)
splitTicks (RightRotation x) = replicate x (RightRotation 1)

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "Part One"
  let (Right rs) = parseInput input
  let pos = applyRotation [50] rs :: [Int]
  print $ foldl (\x y -> x + zeroToOne y) 0 pos

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "Part Two"
  let (Right rs) = parseInput input
  let rs' = rs >>= splitTicks
  let pos = applyRotation [50] rs' :: [Int]
  print $ foldl (\x y -> x + zeroToOne y) 0 pos
