module Main (main) where

import Control.Monad.Except
import Text.ParserCombinators.Parsec

-- Types
data JsonValue
  = JsonArray [JsonValue]
  | JsonObject [(JsonValue, JsonValue)]
  | JsonNumber Int
  | JsonString String
  deriving (Show, Eq)

-- Parsing
skipManySpace :: Parser ()
skipManySpace = skipMany space

parseArray :: Parser JsonValue
parseArray = do
  _ <- char '['
  skipManySpace
  values <- sepBy parseValue (char ',' >> skipManySpace)
  skipManySpace
  _ <- char ']'
  return $ JsonArray values

parseObject :: Parser JsonValue
parseObject = do
  _ <- char '{'
  skipManySpace
  values <- sepBy parseField (char ',' >> skipManySpace)
  skipManySpace
  _ <- char '}'
  return $ JsonObject values

parseField :: Parser (JsonValue, JsonValue)
parseField = do
  skipManySpace
  key <- parseString
  skipManySpace
  _ <- char ':'
  skipManySpace
  value <- parseValue
  skipManySpace
  return (key, value)

parseString :: Parser JsonValue
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ JsonString x

parseNumber :: Parser JsonValue
parseNumber = do
  sign <- option ' ' (oneOf "+-")
  x <- many1 digit
  let mult = if sign == '-' then -1 else 1
  return $ JsonNumber (read x * mult)

parseValue :: Parser JsonValue
parseValue = parseArray <|> parseObject <|> parseNumber <|> parseString

parseJson :: String -> Either String JsonValue
parseJson input = case parse parseValue "json" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Part 1
sumNumbers :: JsonValue -> Int
sumNumbers (JsonNumber x) = x
sumNumbers (JsonArray xs) = sum $ map sumNumbers xs
sumNumbers (JsonObject xs) = sum $ map (sumNumbers . snd) xs
sumNumbers _ = 0

-- Part 2
sumIgnoringRed :: JsonValue -> Int
sumIgnoringRed (JsonNumber x) = x
sumIgnoringRed (JsonArray xs) = sum $ map sumIgnoringRed xs
sumIgnoringRed (JsonObject xs) = if any ((== JsonString "red") . snd) xs then 0 else sum $ map (sumIgnoringRed . snd) xs
sumIgnoringRed _ = 0

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right x) = parseJson input
  print $ sumNumbers x
  print $ sumIgnoringRed x
