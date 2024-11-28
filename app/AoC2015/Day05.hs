module AoC2015.Day05 (main) where

import Data.List (isInfixOf)


boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

hasThreeVowels :: String -> Bool
hasThreeVowels = (>= 3) . length . filter isVowel
  where
    isVowel c = c `elem` "aeiou"

hasDoubleLetter :: Eq a => [a] -> Bool
hasDoubleLetter s = or $ zipWith (==) s (tail s)

doesNotContainBadString :: String -> Bool
doesNotContainBadString s = not $ or $ zipWith isBad s (tail s)
  where
    isBad 'a' 'b' = True
    isBad 'c' 'd' = True
    isBad 'p' 'q' = True
    isBad 'x' 'y' = True
    isBad _ _ = False

isNiceOne :: String -> Bool
isNiceOne s = and $ fmap (\f -> f s) [hasThreeVowels, hasDoubleLetter, doesNotContainBadString]

hasSeparatedRepeat :: Eq a => [a] -> Bool
hasSeparatedRepeat s = or $ zipWith3 (\x _ y -> x == y) s (tail s) (drop 2 s)

hasDoublePair :: String -> Bool
hasDoublePair [] = False
hasDoublePair [_] = False
hasDoublePair (x:y:ys) = isInfixOf [x, y] ys || hasDoublePair (y:ys)

isNiceTwo :: String -> Bool
isNiceTwo s = hasSeparatedRepeat s && hasDoublePair s

main :: IO ()
main = do
  input <- lines <$> readFile "data/input.txt"
  print . sum $ boolToInt . isNiceOne <$> input
  print . sum $ boolToInt . isNiceTwo <$> input
