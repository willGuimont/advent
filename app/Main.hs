{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Advent
    ( defaultAoCOpts,
      runAoC,
      mkDay_,
      AoC(AoCInput),
      AoCOpts,
      AoCUserAgent(..) )
import AoC2025.Day02 ( partOne, partTwo )
import Data.Text (unpack)
import System.Environment (getEnv)

-- Changes daily
year :: Integer
year = 2025

day :: Integer
day = 2

example :: String
example = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

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

