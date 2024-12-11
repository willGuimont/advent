{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Advent
import AoC2024.Day10
import Data.Text (unpack)
import System.Environment (getEnv)

-- Changes daily
year :: Integer
year = 2024

day :: Integer
day = 10

example :: String
example = "89010123\n\
\78121874\n\
\87430965\n\
\96549874\n\
\45678903\n\
\32019012\n\
\01329801\n\
\10456732\n"

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

