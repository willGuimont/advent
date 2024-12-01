module AoC2021.Day09 where

import Data.Bifunctor

main :: IO ()
main = do
  file <- readFile "test.txt"
  let heightMap = map (map (read . (: []))) $ lines file :: [[Int]]
      height = length heightMap
      width = length $ head heightMap
      getAt i j = if i < 0 || j < 0 || i >= width || j >= height then 9 else heightMap !! j !! i
      biggerAround i j = all (> getAt i j) surrouding
        where
          delta = [(-1, 0), (1, 0), (0, -1), (0, 1)]
          surrouding = map (uncurry getAt . bimap (i +) (j +)) delta
      pos = [(i, j) | i <- [0 .. width - 1], j <- [0 .. height - 1]]
      lowPoint = filter (uncurry biggerAround) pos
   in do
        print $ [getAt i j | i <- [0 .. width - 1], j <- [0 .. height - 1]]
        print $ sum $ map ((+ 1) . uncurry getAt) lowPoint
        print $ lowPoint
