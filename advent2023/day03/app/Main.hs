module Main (main) where

day1 :: [String] -> Int
day1 s = go 0 0 0
    where
        rows = length s
        cols = length . head $ s
        go :: Int -> Int -> Int -> Int
        go r c acc = undefined

main :: IO ()
main = do
    input <- readFile "data/sample.txt"
    let xs = lines input
    print xs
