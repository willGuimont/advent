{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module AoC2021.Day02 where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec

-- Types
data Instruction
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

type Plan = [Instruction]

data PlanState = PlanState
  { _depth :: Int,
    _position :: Int,
    _aim :: Int
  }
  deriving (Show)

makeLenses ''PlanState

data EvalPlan a where
  EvalBind :: EvalPlan a -> (a -> EvalPlan b) -> EvalPlan b
  EvalReturn :: a -> EvalPlan a
  EvalSet :: PlanState -> EvalPlan ()
  EvalGet :: EvalPlan PlanState

instance Functor EvalPlan where fmap = liftM

instance Applicative EvalPlan where pure = EvalReturn; (<*>) = ap

instance Monad EvalPlan where (>>=) = EvalBind

-- Parsing
parseForward :: Parser Instruction
parseForward = Forward . read <$> (string "forward" *> many1 space *> many1 digit <* many newline)

parseDown :: Parser Instruction
parseDown = Down . read <$> (string "down" *> many1 space *> many1 digit <* many newline)

parseUp :: Parser Instruction
parseUp = Up . read <$> (string "up" *> many1 space *> many1 digit <* many newline)

parseInstruction :: Parser Instruction
parseInstruction = parseForward <|> parseDown <|> parseUp

parsePlan :: Parser Plan
parsePlan = many parseInstruction

parseInput :: String -> Either String Plan
parseInput input = case parse parsePlan "plan" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Execution
exec :: PlanState -> EvalPlan a -> (PlanState, a)
exec s = \case
  EvalBind ma f ->
    let (s', a) = exec s ma
     in exec s' (f a)
  EvalReturn v -> (s, v)
  EvalSet s' -> (s', ())
  EvalGet -> (s, s)

evalOne :: Instruction -> EvalPlan ()
evalOne = \case
  Forward f -> do
    EvalGet >>= EvalSet . over position (+ f)
  Down d -> do
    EvalGet >>= EvalSet . over depth (+ d)
  Up u -> do
    EvalGet >>= EvalSet . over depth (\d -> d - u)

evalTwo :: Instruction -> EvalPlan ()
evalTwo = \case
  Forward f -> do
    EvalGet >>= EvalSet . update
    where
      update s =
        let a = view aim s
         in over position (+ f) $ over depth (+ (f * a)) s
  Down d -> do
    EvalGet >>= EvalSet . over aim (+ d)
  Up u -> do
    EvalGet >>= EvalSet . over aim (\d -> d - u)

evalPlan :: (Instruction -> EvalPlan ()) -> PlanState -> Plan -> (PlanState, PlanState)
evalPlan _ s [] = exec s EvalGet
evalPlan eval s (x : xs) = let (s', _) = exec s (eval x) in evalPlan eval s' xs

-- Main
main :: IO ()
main = do
  input <- readFile "input.txt"
  let plan = fromRight [] $ parseInput input
      (s, _) = evalPlan evalOne (PlanState 0 0 0) plan
      d = view depth s
      p = view position s
   in print $ d * p
  let plan = fromRight [] $ parseInput input
      (s, _) = evalPlan evalTwo (PlanState 0 0 0) plan
      d = view depth s
      p = view position s
   in print $ d * p
