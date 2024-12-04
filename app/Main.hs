{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnv)
import Data.Text (unpack)
import Advent

import AoC2024.Day04

-- Changes daily
year :: Integer
year = 2024

day :: Integer
day = 4

example :: String
example = "MMMSXXMASM\n\
\MSAMXMSMSA\n\
\AMXSXMAAMM\n\
\MSAMASMSMX\n\
\XMASAMXAMM\n\
\XXAMMXXAMA\n\
\SMSMSASXSS\n\
\SAXAMASAAA\n\
\MAMMMXMMMM\n\
\MXMXAXMASX\n"

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
    putStrLn "### Example ###"
    _ <- partOne example
    _ <- partTwo example

    let input' = unpack input
    putStrLn "### Real input ###"
    _ <- partOne input'
    _ <- partTwo input'

    return ()

