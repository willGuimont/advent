{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Advent
    ( defaultAoCOpts,
      runAoC,
      mkDay_,
      AoC(AoCInput),
      AoCOpts,
      AoCUserAgent(..) )
import AoC2025.Day03 ( partOne, partTwo )
import Data.Text (unpack)
import System.Environment (getEnv)

-- Changes daily
year :: Integer
year = 2025

day :: Integer
day = 3

example :: String
example = "987654321111111\n\
\811111111111119\n\
\234234234234278\n\
\818181911112111"

-- AoC wrapper
getSession :: IO String
getSession = getEnv "AOC_TOKEN"

userAgent :: AoCUserAgent
userAgent = AoCUserAgent {_auaRepo = "github.com/willGuimont/advent", _auaEmail = "william.guimont-martin@norlab.ulaval.ca"}

adventOptions :: IO AoCOpts
adventOptions = do
  defaultAoCOpts userAgent year <$> getSession

main :: IO ()
main = do
  putStrLn "\n### Example ###"
  _ <- partOne example
  _ <- partTwo example

  opt <- adventOptions
  (Right input) <- runAoC opt $ AoCInput (mkDay_ day)
  let input' = unpack input
  putStrLn "\n### Real input ###"
  _ <- partOne input'
  _ <- partTwo input'

  return ()

