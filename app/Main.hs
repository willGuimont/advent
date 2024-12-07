{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnv)
import Data.Text (unpack)
import Advent

import AoC2024.Day07

-- Changes daily
year :: Integer
year = 2024

day :: Integer
day = 7

example :: String
example = "190: 10 19\n\
\3267: 81 40 27\n\
\83: 17 5\n\
\156: 15 6\n\
\7290: 6 8 6 15\n\
\161011: 16 10 13\n\
\192: 17 8 14\n\
\21037: 9 7 18 13\n\
\292: 11 6 16 20\n"

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

