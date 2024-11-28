{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getEnv)
import Data.Text (pack)
import Advent

year :: Integer
year = 2022

-- Session from .env file
getSession :: IO String
getSession = getEnv "AOC_TOKEN"

userAgent :: AoCUserAgent
userAgent = AoCUserAgent { _auaRepo = "github.com/willGuimont/advent", _auaEmail = "william.guimont-martin@norlab.ulaval.ca" }

adventOptions :: IO AoCOpts
adventOptions = do
    session <- getSession
    return $ defaultAoCOpts userAgent year session

main :: IO ()
main = do
    opt <- adventOptions
    print $ show opt
    print "\n"
    res <- runAoC opt $ AoCInput (mkDay_ 5)
    print . show $ res
