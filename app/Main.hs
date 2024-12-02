{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnv)
import Data.Text (unpack)
import Advent

import AoC2024.Day02

-- Changes daily
year :: Integer
year = 2024

day :: Integer
day = 2

example :: String
example = "7 6 4 2 1\n\
\1 2 7 8 9\n\
\9 7 6 2 1\n\
\1 3 2 4 5\n\
\8 6 4 4 1\n\
\1 3 6 7 9\n"

-- AoC wrapper
getSession :: IO String
getSession = getEnv "AOC_TOKEN"

userAgent :: AoCUserAgent
userAgent = AoCUserAgent { _auaRepo = "github.com/willGuimont/advent", _auaEmail = "william.guimont-martin@norlab.ulaval.ca" }

adventOptions :: IO AoCOpts
adventOptions = do
    defaultAoCOpts userAgent year <$> getSession

main :: IO ()
main = do
    opt <- adventOptions
    (Right input) <- runAoC opt $ AoCInput (mkDay_ day)
    putStrLn "Example"
    _ <- partOne example
    _ <- partTwo example

    -- let input' = unpack input
    -- putStrLn "Real input"
    -- _ <- partOne input'
    -- _ <- partTwo input'

    return ()
