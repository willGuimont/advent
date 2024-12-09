module AoC2021.Day06 where

import Data.List.Split
import Data.MultiSet qualified as M

-- Naive solution
step :: Int -> Int
step 0 = 6
step x = x - 1

stepAll :: [Int] -> [Int]
stepAll xs = map step xs <> replicate numNew 8
  where
    numNew = length $ filter (== 0) xs

naive :: Int -> [Int] -> Int
naive d lanterns = length (iterate stepAll lanterns !! d)

-- A better solution
smarter :: M.MultiSet Integer -> M.MultiSet Integer
smarter = M.concatMap (\i -> if i == 0 then [6, 8] else [i - 1])

-- Main
main :: IO ()
main = do
  input <- readFile "input.txt"
  let lanterns = map read $ splitOn "," input :: [Integer]
   in do
        print $ naive 80 (map fromInteger lanterns)
        print $ M.size $ (!! 256) $ iterate smarter $ M.fromList lanterns
