module Main (main) where

import Control.Applicative ((<|>))
import Data.List (elemIndex, group)
import Data.Maybe (fromMaybe)

increment :: Char -> Char
increment 'z' = 'a'
increment c = succ c

incrementSkippingIllegalLetters :: String -> String
incrementSkippingIllegalLetters s = fromMaybe (reverse . go . reverse $ s) next
  where
    illegalIndex = elemIndex 'i' s <|> elemIndex 'o' s <|> elemIndex 'l' s
    next = illegalIndex >>= \i -> Just (take i s <> [increment (s !! i)] <> replicate (length s - i - 1) 'a')
    go [] = []
    go (x : xs) = n : if n /= 'a' then xs else go xs
      where
        n = increment x

hasThreeSuccessiveLetters :: String -> Bool
hasThreeSuccessiveLetters [] = False
hasThreeSuccessiveLetters [_] = False
hasThreeSuccessiveLetters [_, _] = False
hasThreeSuccessiveLetters (x : y : z : rest) = succ x == y && succ y == z || hasThreeSuccessiveLetters (y : z : rest)

twoOverlappingPairs :: String -> Bool
twoOverlappingPairs s = length (filter id ((>= 2) . length <$> group s)) >= 2

illegalNextPassword :: String -> String
illegalNextPassword = head . filter (\p -> twoOverlappingPairs p && hasThreeSuccessiveLetters p) . drop 1 . iterate incrementSkippingIllegalLetters

main :: IO ()
main = do
  print $ illegalNextPassword "abcdefgh"
  print $ illegalNextPassword "ghijklmn"
  let newPassword = illegalNextPassword "hxbxwxba"
  print newPassword
  print $ illegalNextPassword newPassword
