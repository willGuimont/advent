module Main (main) where

import qualified Data.List as L
import Data.List.Split

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let xs = lines input
  let by_elf = (sum . fmap read <$> splitOn [""] xs) :: [Int]
  print . maximum $ by_elf
  print . sum . take 3 . reverse . L.sort $ by_elf
