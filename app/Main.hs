{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnv)
import Data.Text (unpack)
import Advent

import AoC2024.Day05

-- Changes daily
year :: Integer
year = 2024

day :: Integer
day = 5

example :: String
example = "47|53\n\
\97|13\n\
\97|61\n\
\97|47\n\
\75|29\n\
\61|13\n\
\75|53\n\
\29|13\n\
\97|29\n\
\53|29\n\
\61|53\n\
\97|53\n\
\61|29\n\
\47|13\n\
\75|47\n\
\97|75\n\
\47|61\n\
\75|61\n\
\47|29\n\
\75|13\n\
\53|13\n\
\\n\
\75,47,61,53,29\n\
\97,61,53,29,13\n\
\75,29,13\n\
\75,97,47,61,53\n\
\61,13,29\n\
\97,13,75,29,47"

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

    -- let input' = unpack input
    -- putStrLn "### Real input ###"
    -- _ <- partOne input'
    -- _ <- partTwo input'

    return ()

