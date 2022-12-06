module Main (main) where

import Control.Monad.Except (throwError)
import Data.List (sort)
import Text.ParserCombinators.Parsec

-- Types
newtype BoxSize = BoxSize (Int, Int, Int) deriving (Show)

-- Parsing
listToTriple :: [a] -> (a, a, a)
listToTriple [x, y, z] = (x, y, z)
listToTriple _ = undefined

parseBox :: Parser BoxSize
parseBox = BoxSize . listToTriple . fmap read <$> many1 digit `sepBy` char 'x'

parseBoxes :: Parser [BoxSize]
parseBoxes = parseBox `sepBy` newline

parseInput :: String -> Either String [BoxSize]
parseInput input = case parse parseBoxes "boxes" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
getPaperQuantity :: BoxSize -> Int
getPaperQuantity (BoxSize (w, h, l)) = 2 * (a + b + c) + minimum [a, b, c]
  where
    a = l * w
    b = w * h
    c = h * l

getRibbonQuantity :: BoxSize -> Int
getRibbonQuantity (BoxSize (w, h, l)) = 2 * (a + b) + h * w * l
  where
    [a, b, _] = sort [w, h, l]

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right boxes) = parseInput input
  print . sum $ getPaperQuantity <$> boxes
  print . sum $ getRibbonQuantity <$> boxes
