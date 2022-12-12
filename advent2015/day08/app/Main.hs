module Main (main) where

import Control.Monad.Except (throwError)
import Data.Char (chr)
import Text.ParserCombinators.Parsec

-- Parsing
parseEscapedQuote :: Parser Char
parseEscapedQuote = string "\\\"" >> return '"'

parseHex :: Parser Char
parseHex = do
  _ <- string "\\x"
  x <- anyChar
  y <- anyChar
  return ' '

parseEscapedEscape :: Parser Char
parseEscapedEscape = string "\\\\" >> return '\\'

parseChar :: Parser Char
parseChar = try parseEscapedQuote <|> try parseHex <|> parseEscapedEscape <|> letter

parseString :: Parser String
parseString = do
  _ <- char '\"'
  content <- many parseChar
  _ <- char '\"'
  return $ "\"" <> content <> "\""

parseInput :: String -> Either String [String]
parseInput s = case parse (parseString `sepBy` newline) "input" s of
  Left err -> throwError $ show err
  Right x -> return x

encode :: String -> String
encode s = "\"" <> go s <> "\""
  where
    go ('\\' : xs) = "\\\\" <> go xs
    go ('"' : xs) = "\\\"" <> go xs
    go (x : xs) = [x] <> go xs
    go [] = ""

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let literalSize = sum $ length <$> lines input
      (Right strings) = parseInput input
      parsedSize = sum $ subtract 2 . length <$> strings
      representationSize = sum $ length . encode <$> lines input
  print $ literalSize - parsedSize
  print $ representationSize - literalSize
