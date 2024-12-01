module AoC2021.Day01 where

dayOne :: [Int] -> Int
dayOne xs = sum $ zipWith (\x y -> fromEnum $ x < y) xs (drop 1 xs)

main :: IO ()
main = do
  file <- readFile "input.txt"
  let xs = ((map read $ words file) :: [Int])
   in do
        print $ dayOne xs
        print $ dayOne $ zipWith3 (\x y z -> x + y + z) xs (drop 1 xs) (drop 2 xs)
