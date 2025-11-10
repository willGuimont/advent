{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Advent
import AoC2025.Day01
import Data.Text (unpack)
import System.Environment (getEnv)

-- Changes daily
year :: Integer
year = 2025

day :: Integer
day = 1

example :: String
example = "125 17\n"

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
  putStrLn "### Example ###"
  _ <- partOne example
  _ <- partTwo example

  opt <- adventOptions
  (Right input) <- runAoC opt $ AoCInput (mkDay_ day)
  let input' = unpack input
  putStrLn "### Real input ###"
  _ <- partOne input'
  _ <- partTwo input'

  return ()

