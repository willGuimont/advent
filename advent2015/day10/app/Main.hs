module Main (main) where

import Data.List (group)

step :: String -> String
step "" = ""
step s = concatMap (\g -> show (length g) ++ [head g]) $ group s

getLengthAtStep :: Int -> String -> Int
getLengthAtStep n s = length $ iterate step s !! n

main :: IO ()
main = do
  let input = "1113122113"
  print $ getLengthAtStep 40 input
  print $ getLengthAtStep 50 input
