module AoC2022.Day06 (main) where

import Data.List (elemIndex, nub, tails)
import Data.Maybe (fromJust)

allDifferent :: Eq a => [a] -> Bool
allDifferent xs = length xs == length (nub xs)

window :: Int -> [a] -> [[a]]
window m = foldr (zipWith (:)) (repeat []) . take m . tails

uniqueSubseqEnd :: Int -> String -> Int
uniqueSubseqEnd m s = m + (fromJust . elemIndex True $ allDifferent <$> window m s)

findBeginningPacket :: String -> Int
findBeginningPacket = uniqueSubseqEnd 4

findBeginningMessage :: String -> Int
findBeginningMessage = uniqueSubseqEnd 14

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  print $ findBeginningPacket input
  print $ findBeginningMessage input
