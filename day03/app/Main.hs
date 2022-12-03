module Main (main) where

import Control.Monad.State
import Data.Char (isLower, isUpper, ord)
import Data.List (intersect, nub)
import Data.List.Split
import qualified Data.Map as M

splitCompartment :: String -> (String, String)
splitCompartment x = splitAt splitIdx x
  where
    splitIdx = length x `div` 2

findDuplicate :: (String, String) -> Char
findDuplicate = head . uncurry intersect

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = 0

getNumPerItem :: [String] -> State (M.Map Char Int) ()
getNumPerItem = mapM_ addItems
  where
    addItems :: String -> State (M.Map Char Int) ()
    addItems s = mapM_ addItem $ nub s
    addItem :: Char -> State (M.Map Char Int) ()
    addItem c = do
      m <- get
      put $ if c `M.member` m then M.adjust (+ 1) c m else M.insert c 1 m

getBadges :: [[String]] -> [Char]
getBadges perGroup = fmap (fst . head . M.assocs) uniques
  where
    maps = fmap (\x -> execState (getNumPerItem x) M.empty) perGroup
    uniques = fmap (M.filter (== 3)) maps

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let perRucksack = lines input
  let perCompartment = splitCompartment <$> perRucksack
  print . sum $ priority . findDuplicate <$> perCompartment
  let perGroup = chunksOf 3 perRucksack
  print . sum $ priority <$> getBadges perGroup
