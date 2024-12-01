{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnv)
import Data.Text (unpack)
import Advent

import AoC2024.Day01

year :: Integer
year = 2024

-- Session from .env file
getSession :: IO String
getSession = getEnv "AOC_TOKEN"

userAgent :: AoCUserAgent
userAgent = AoCUserAgent { _auaRepo = "github.com/willGuimont/advent", _auaEmail = "william.guimont-martin@norlab.ulaval.ca" }

adventOptions :: IO AoCOpts
adventOptions = do
    defaultAoCOpts userAgent year <$> getSession

example :: String
example = "3   4\n\
\4   3\n\
\2   5\n\
\1   3\n\
\3   9\n\
\3   3\n"

main :: IO ()
main = do
    opt <- adventOptions
    (Right input) <- runAoC opt $ AoCInput (mkDay_ 1)
    putStrLn "Example"
    _ <- partOne example
    _ <- partTwo example

    let input' = unpack input
    putStrLn "Real input"
    _ <- partOne input'
    _ <- partTwo input'

    return ()
