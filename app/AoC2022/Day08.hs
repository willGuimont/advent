{-# LANGUAGE TemplateHaskell #-}

module AoC2022.Day08 (main) where

import Control.Lens
import Control.Monad.State
import Data.Set qualified as S

-- Types
newtype World = World {_visible :: S.Set (Int, Int)} deriving (Show)

makeLenses ''World

-- logic
addVisibles :: ((Int, Int) -> (Int, Int)) -> [[Int]] -> State World ()
addVisibles coordsTrans forest = do
  let vs = visibleRow <$> coords
  mapM_ (\v -> get >>= put . over visible (S.union (S.fromList v))) vs
  where
    forestSize = length forest
    pos = [0 .. forestSize - 1]
    coords = fmap coordsTrans <$> [[(x, y) | y <- pos] | x <- pos]
    getAt (x, y) = forest !! x !! y
    visibleRow = snd . foldl (\(m, ts) p -> if getAt p > m then (getAt p, p : ts) else (m, ts)) (-1, [])

flipY :: Int -> (Int, Int) -> (Int, Int)
flipY size (x, y) = (x, size - y - 1)

swapFlipY :: Int -> (Int, Int) -> (Int, Int)
swapFlipY size (x, y) = (size - y - 1, x)

swap :: (Int, Int) -> (Int, Int)
swap (x, y) = (y, x)

getVisibles :: [[Int]] -> S.Set (Int, Int)
getVisibles forest = view visible . execState run $ World S.empty
  where
    forestSize = length forest
    run = do
      _ <- addVisibles id forest
      _ <- addVisibles (flipY forestSize) forest
      _ <- addVisibles swap forest
      _ <- addVisibles (swapFlipY forestSize) forest
      return ()

scene :: (Int, Int) -> [[Int]] -> (Int, Int) -> Int
scene p forest (dx, dy) = fst $ foldl (\(count, stop) x -> if not stop then (count + 1, stop || x >= currTree) else (count, stop)) (0, False) trees
  where
    forestSize = length forest
    positions = drop 1 $ takeWhile (\(x, y) -> x >= 0 && y >= 0 && x < forestSize && y < forestSize) $ iterate (\(x, y) -> (x + dx, y + dy)) p
    trees = (\(x, y) -> forest !! x !! y) <$> positions
    currTree = forest !! fst p !! snd p

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore forest p = product . fmap (scene p forest) $ dpos
  where
    dpos = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], abs (x + y) == 1]

printForestVisible :: [[Int]] -> S.Set (Int, Int) -> IO ()
printForestVisible f vs = do
  let forestSize = length f
      pos = [0 .. forestSize - 1]
      coords = [[(x, y) | y <- pos] | x <- pos]
      printRow row = do
        mapM_ (\(x, y) -> let str = if (x, y) `S.member` vs then "[" <> show (f !! x !! y) <> "]" else " " <> show (f !! x !! y) <> " " in putStr str) row
        putStr "\n"
  putStrLn "-----"
  mapM_ printRow coords
  putStrLn "-----"

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let forest = (fmap (read . (: [])) <$> lines input) :: [[Int]]
      vs = getVisibles forest
      forestSize = length forest

  printForestVisible forest vs
  print $ S.size vs

  print . maximum $ scenicScore forest <$> [(x, y) | x <- [0 .. forestSize - 1], y <- [0 .. forestSize - 1]]
