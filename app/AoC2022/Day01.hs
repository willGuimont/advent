module AoC2022.Day01 (main) where

import Data.List (sortBy)
import Data.List.Split
import Data.Ord (Down (Down), comparing)

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let xs = lines input
  let by_elf = (sum . fmap read <$> splitOn [""] xs) :: [Int]
  print . maximum $ by_elf
  print . sum . take 3 . sortBy (comparing Down) $ by_elf
