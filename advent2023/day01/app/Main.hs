module Main (main) where

import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex)
import Data.Maybe (maybeToList)

firstDigit :: String -> Int
firstDigit s = digitToInt $ head $ filter isDigit s

isPrefixOf :: String -> String -> Bool
isPrefixOf p s = take (length p) s == p

digitStrings :: [String]
digitStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

toDigits :: [Char] -> [Int]
toDigits "" = []
toDigits xs@(s : ss) =
  ( if isDigit s
      then [digitToInt s]
      else parsed
  )
    ++ toDigits ss
  where
    isPrefix = map (`isPrefixOf` xs) digitStrings
    parsed = maybeToList $ (+ 1) <$> elemIndex True isPrefix

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let xs = lines input
  let fstDigits = map firstDigit xs
  let lstDigits = map (firstDigit . reverse) xs
  let vals = zipWith (\x y -> read (show x ++ show y)) fstDigits lstDigits :: [Int]
  print $ sum vals
  let digits = map toDigits xs
  let vals2 = map (\x -> read $ concatMap show [head x, last x]) digits :: [Int]
  print $ sum vals2
