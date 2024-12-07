{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnv)
import Data.Text (unpack)
import Advent

import AoC2024.Day08

-- Changes daily
year :: Integer
year = 2024

day :: Integer
day = 8

example :: String
example = "............\n\
\........0...\n\
\.....0......\n\
\.......0....\n\
\....0.......\n\
\......A.....\n\
\............\n\
\............\n\
\........A...\n\
\.........A..\n\
\............\n\
\............\n"

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

