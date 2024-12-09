{-# LANGUAGE TemplateHaskell #-}

module AoC2015.Day06 (main) where

import Control.Lens
import Control.Monad.Except
import Data.Map qualified as M
import Data.Set qualified as S
import Text.ParserCombinators.Parsec

-- Types
data Switch
  = TurnOn
  | TurnOff
  | Toggle
  deriving (Show)

data Instruction = Instruction
  { _switch :: Switch,
    _fromPos :: (Int, Int),
    _toPos :: (Int, Int)
  }
  deriving (Show)

makeLenses ''Instruction

type World = S.Set (Int, Int)

type WorldBright = M.Map (Int, Int) Int

-- Parsing
parseOn :: Parser Switch
parseOn = string "turn on" >> return TurnOn

parseOff :: Parser Switch
parseOff = string "turn off" >> return TurnOff

parseToggle :: Parser Switch
parseToggle = string "toggle" >> return Toggle

parseSwitch :: Parser Switch
parseSwitch = try parseOn <|> try parseOff <|> parseToggle

parsePos :: Parser (Int, Int)
parsePos = do
  x <- read <$> many1 digit
  _ <- char ','
  y <- read <$> many1 digit
  return (x, y)

parseInstruction :: Parser Instruction
parseInstruction = do
  switch' <- parseSwitch
  _ <- space
  fromPos' <- parsePos
  _ <- space
  _ <- string "through"
  _ <- space
  Instruction switch' fromPos' <$> parsePos

parseInstructionPlan :: Parser [Instruction]
parseInstructionPlan = parseInstruction `sepBy` newline

parseInput :: String -> Either String [Instruction]
parseInput input = case parse parseInstructionPlan "instructions" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
applyInstruction :: World -> Switch -> (Int, Int) -> World
applyInstruction w TurnOn p = S.insert p w
applyInstruction w TurnOff p = S.delete p w
applyInstruction w Toggle p = if p `S.member` w then applyInstruction w TurnOff p else applyInstruction w TurnOn p

applyInstructionPlan :: [Instruction] -> World -> World
applyInstructionPlan is w = foldl apply w is
  where
    apply :: World -> Instruction -> World
    apply w' i = foldl (\w'' p -> applyInstruction w'' (view switch i) p) w' $ positions (view fromPos i) (view toPos i)
    positions (fx, fy) (tx, ty) = (,) <$> [fx .. tx] <*> [fy .. ty]

addWithDefault :: (Ord k) => k -> Int -> M.Map k Int -> M.Map k Int
addWithDefault k v m = minZero
  where
    newMap = if k `M.member` m then M.update (return . (+ v)) k m else M.insert k v m
    minZero = newMap & at k %~ (\(Just x) -> return $ max 0 x)

applyBrightInstruction :: WorldBright -> Switch -> (Int, Int) -> WorldBright
applyBrightInstruction w TurnOn p = addWithDefault p 1 w
applyBrightInstruction w TurnOff p = addWithDefault p (-1) w
applyBrightInstruction w Toggle p = addWithDefault p 2 w

applyBrightInstructionPlan :: [Instruction] -> WorldBright -> WorldBright
applyBrightInstructionPlan is w = foldl apply w is
  where
    apply :: WorldBright -> Instruction -> WorldBright
    apply w' i = foldl (\w'' p -> applyBrightInstruction w'' (view switch i) p) w' $ positions (view fromPos i) (view toPos i)
    positions (fx, fy) (tx, ty) = (,) <$> [fx .. tx] <*> [fy .. ty]

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  print $ parseInput input
  let (Right instructions) = parseInput input
  print . S.size $ applyInstructionPlan instructions S.empty
  print . sum . applyBrightInstructionPlan instructions $ M.empty
