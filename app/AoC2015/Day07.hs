module AoC2015.Day07 (main) where

import Control.Monad.Except
import qualified Control.Monad.State as S
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Word (Word16)
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)

-- Types
type Variable = String

type World = M.Map Variable Word16

data Instruction
  = Provide Variable Variable
  | And Variable Variable Variable
  | Or Variable Variable Variable
  | Lshift Variable Int Variable
  | Rshift Variable Int Variable
  | Not Variable Variable
  deriving (Show)

-- Parsing
parseVariable :: Parser Variable
parseVariable = many1 alphaNum

parseProvide :: Parser Instruction
parseProvide = do
  var <- parseVariable
  _ <- string " -> "
  name <- parseVariable
  return $ Provide var name

parseAnd :: Parser Instruction
parseAnd = do
  op1 <- parseVariable
  _ <- string " AND "
  op2 <- parseVariable
  _ <- string " -> "
  out <- parseVariable
  return $ And op1 op2 out

parseOr :: Parser Instruction
parseOr = do
  op1 <- parseVariable
  _ <- string " OR "
  op2 <- parseVariable
  _ <- string " -> "
  out <- parseVariable
  return $ Or op1 op2 out

parseLShift :: Parser Instruction
parseLShift = do
  op <- parseVariable
  _ <- string " LSHIFT "
  val <- read <$> many1 digit
  _ <- string " -> "
  out <- parseVariable
  return $ Lshift op val out

parseRShift :: Parser Instruction
parseRShift = do
  op <- parseVariable
  _ <- string " RSHIFT "
  val <- read <$> many1 digit
  _ <- string " -> "
  out <- parseVariable
  return $ Rshift op val out

parseNot :: Parser Instruction
parseNot = do
  _ <- string "NOT "
  name <- parseVariable
  _ <- string " -> "
  out <- parseVariable
  return $ Not name out

parseInstruction :: Parser Instruction
parseInstruction = try parseProvide <|> try parseAnd <|> try parseOr <|> try parseLShift <|> try parseRShift <|> try parseNot

parseInstructions :: Parser [Instruction]
parseInstructions = parseInstruction `sepBy` newline

parseInput :: String -> Either String [Instruction]
parseInput input = case parse parseInstructions "instructions" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
getRequirements :: World -> [Variable] -> Maybe [Word16]
getRequirements w = mapM getValue
  where
    getValue v = case readMaybe v of
      Just cte -> return cte
      _ -> M.lookup v w

apply :: Variable -> [Variable] -> ([Word16] -> Word16) -> S.State World ()
apply out reqs f =
  S.get >>= \w -> case getRequirements w reqs of
    Just xs -> S.put $ M.insert out (f xs) w
    _ -> return ()

applyInstruction :: Instruction -> S.State World ()
applyInstruction (Provide var out) = apply out [var] (foldl1 const)
applyInstruction (And op1 op2 out) = apply out [op1, op2] (foldl1 (.&.))
applyInstruction (Or op1 op2 out) = apply out [op1, op2] (foldl1 (.|.))
applyInstruction (Lshift var amount out) = apply out [var] (foldl1 const . fmap (`shiftL` amount))
applyInstruction (Rshift var amount out) = apply out [var] (foldl1 const . fmap (`shiftR` amount))
applyInstruction (Not var out) = apply out [var] (foldl1 const . fmap complement)

applyInstructions :: [Instruction] -> S.State World ()
applyInstructions = mapM_ applyInstruction

solve :: [Instruction] -> World -> World
solve is = S.execState (applyInstructions is)

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = if p x then while p f (f x) else x

outputsTo :: Variable -> Instruction -> Bool
outputsTo v (Provide _ out) = v == out
outputsTo v (And _ _ out) = v == out
outputsTo v (Or _ _ out) = v == out
outputsTo v (Lshift _ _ out) = v == out
outputsTo v (Rshift _ _ out) = v == out
outputsTo v (Not _ out) = v == out

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  print $ parseInput input
  let (Right instructions) = parseInput input
  let endState = while (isNothing . M.lookup "a") (solve instructions) M.empty
  let (Just signalA) = M.lookup "a" endState
  print signalA

  let rewired = Provide (show signalA) "b" : filter (not . outputsTo "b") instructions
  let rewiredEndState = while (isNothing . M.lookup "a") (solve rewired) M.empty
  print $ M.lookup "a" rewiredEndState
