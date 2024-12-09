{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnv)
import Data.Text (unpack)
import Advent

import AoC2024.Day09

-- Changes daily
year :: Integer
year = 2024

day :: Integer
day = 9

example :: String
example = "2333133121414131402\n"

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

