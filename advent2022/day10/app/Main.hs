{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Control.Monad.Except (throwError)
import qualified Data.List.Split as LS
import Text.ParserCombinators.Parsec

-- Types
data Instruction
  = AddX Int
  | NoOp
  deriving (Show)

-- Parsing
parseSignedInt :: Parser Int
parseSignedInt = char '-' >> negate . read <$> many1 digit

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseAddX :: Parser [Instruction]
parseAddX = string "addx " >> (\v -> [NoOp, AddX v]) <$> (parseSignedInt <|> parseInt)

parseNoOp :: Parser [Instruction]
parseNoOp = string "noop" >> return [NoOp]

parseInstruction :: Parser [Instruction]
parseInstruction = parseAddX <|> parseNoOp

parseProgram :: Parser [Instruction]
parseProgram = concat <$> parseInstruction `sepBy` newline

parseInput :: String -> Either String [Instruction]
parseInput s = case parse parseProgram "program" s of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
data Computer = Computer
  { _regX :: Int,
    _cycles :: Int
  }
  deriving (Show)

makeLenses ''Computer

waitCycles :: Int -> Computer -> Computer
waitCycles v = over cycles (+ v)

addRegisterX :: Int -> Computer -> Computer
addRegisterX v = over regX (+ v)

execInstruction :: Instruction -> Computer -> Computer
execInstruction (AddX v) = addRegisterX v . waitCycles 1
execInstruction NoOp = waitCycles 1

signalStrength :: Computer -> Int
signalStrength c = view cycles c * view regX c

isPixelLit :: Int -> Computer -> Bool
isPixelLit p c = abs ((p `mod` 40) - x) <= 1
  where
    x = view regX c

toPixel :: Bool -> Char
toPixel False = '.'
toPixel True = '#'

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right program) = parseInput input
      initComputer = Computer 1 1
      states = scanl (flip execInstruction) initComputer program
      signal = filter (\s -> let n = view cycles s in (n - 20) `mod` 40 == 0) states
  print . sum $ signalStrength <$> signal

  let cs = subtract 1 . view cycles <$> states
      pixels = toPixel . uncurry isPixelLit <$> zip cs states
      screen = LS.chunksOf 40 pixels
  mapM_ putStrLn screen
