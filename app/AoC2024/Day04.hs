module AoC2024.Day04 (partOne, partTwo) where

import Data.List (transpose)
import Relude.Functor.Fmap (flap)

-- Logic
countSubstring :: Eq a => [a] -> [a] -> Int
countSubstring [] _ = 0
countSubstring _ [] =  0
countSubstring sub str
  | length sub > length str = 0
  | take (length sub) str == sub = 1 + countSubstring sub (drop 1 str)
  | otherwise = countSubstring sub (drop 1 str)

countAllSub :: Eq a => [a] -> [[a]] -> Int
countAllSub sub strs = forward + backward
  where
    forward = sum $ countSubstring sub <$> strs
    backward = sum $ countSubstring (reverse sub) <$> strs

allDiagonals :: [[a]] -> [[a]]
allDiagonals matrix = primaryDiagonals ++ secondaryDiagonals
  where
    numRows = length matrix
    numCols = if null matrix then 0 else length (head matrix)
    primaryDiagonals = [ [ matrix !! (i + k) !! k | k <- [0 .. min (numCols - 1) (numRows - i - 1)] ] 
                       | i <- [0 .. numRows - 1] ]
                    ++ [ [ matrix !! k !! (j + k) | k <- [0 .. min (numRows - 1) (numCols - j - 1)] ] 
                       | j <- [1 .. numCols - 1] ]
    secondaryDiagonals = [ [ matrix !! (i + k) !! (numCols - k - 1) | k <- [0 .. min (numCols - 1) (numRows - i - 1)] ] 
                         | i <- [0 .. numRows - 1] ]
                      ++ [ [ matrix !! k !! (j - k) | k <- [0 .. min (numRows - 1) j] ] 
                         | j <- [numCols - 2, numCols - 3 .. 0] ]

extractWindow :: [[a]] -> Int -> Int -> Int -> Int -> [[a]]
extractWindow matrix x y kx ky =
  take ky $ map (take kx . drop x) $ drop y matrix

subMatrices :: [[a]] -> Int -> Int -> [[[a]]]
subMatrices matrix kx ky = concat [[extractWindow matrix x y kx ky | x <- [0..ox]] | y <- [0..oy]]
  where
    nx = length matrix
    ny = length $ head matrix
    ox = nx - kx
    oy = ny - ky

isXmas :: [[Char]] -> Bool
isXmas xs = isMas prim && isMas sec
  where
    prim = [xs !! i !! i | i <- [0..2]]
    sec = [xs !! i !! (2 - i) | i <- [0..2]]
    isMas x = x == "MAS" || x == "SAM"

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

-- Parts       
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let ls = lines input
  print . sum $ countAllSub "XMAS" <$> flap [id, transpose, allDiagonals] ls

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let ls = lines input
  print . sum . fmap boolToInt $ isXmas <$> subMatrices ls 3 3

