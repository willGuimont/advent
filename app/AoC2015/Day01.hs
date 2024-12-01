module AoC2015.Day01 (main) where

import Data.List (elemIndex)

toLevelChange :: Char -> Int
toLevelChange '(' = 1
toLevelChange ')' = -1
toLevelChange _ = 0

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  print . sum . map toLevelChange $ input
  print . fmap (+ 1) . elemIndex (-1) . scanl1 (+) $ toLevelChange <$> input
