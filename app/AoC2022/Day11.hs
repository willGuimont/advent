{-# LANGUAGE TemplateHaskell #-}

module AoC2022.Day11 (main) where

import Control.Lens
import Control.Monad.Except (throwError)
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

-- Types
type Item = Integer

type MonkeyId = Int

data OperationExpr
  = OldExpr
  | ScalarExpr Integer
  deriving (Show)

data Operation
  = OperationMult OperationExpr OperationExpr
  | OperationAdd OperationExpr OperationExpr
  deriving (Show)

type DivisibleTestValue = Integer

data Monkey = Monkey
  { _monkeyId :: MonkeyId,
    _heldItems :: [Item],
    _operation :: Operation,
    _testDivisor :: DivisibleTestValue,
    _ifTrue :: MonkeyId,
    _ifFalse :: MonkeyId
  }
  deriving (Show)

makeLenses ''Monkey

-- Parsing
indent :: Parser String
indent = string "  "

integer :: Parser Integer
integer = read <$> many1 digit

int :: Parser Int
int = read <$> many1 digit

parseOldExpr :: Parser OperationExpr
parseOldExpr = string "old" >> return OldExpr

parseScalarExpr :: Parser OperationExpr
parseScalarExpr = ScalarExpr <$> integer

parseOperationExpr :: Parser OperationExpr
parseOperationExpr = parseOldExpr <|> parseScalarExpr

parseOperation :: Parser Operation
parseOperation = do
  expr1 <- parseOperationExpr
  _ <- char ' '
  operator <- anyChar
  _ <- char ' '
  expr2 <- parseOperationExpr
  return $ if operator == '*' then OperationMult expr1 expr2 else OperationAdd expr1 expr2

parseMonkey :: Parser Monkey
parseMonkey = do
  monId <- string "Monkey " *> int <* string ":\n"
  holdItemWorries <- indent *> string "Starting items: " *> integer `sepBy` string ", " <* newline
  itemOp <- indent *> string "Operation: new = " *> parseOperation <* newline
  test <- indent *> string "Test: divisible by " *> integer <* newline
  toMonkeyTrue <- indent *> indent *> string "If true: throw to monkey " *> int <* newline
  toMonkeyFalse <- indent *> indent *> string "If false: throw to monkey " *> int <* newline

  return $ Monkey monId holdItemWorries itemOp test toMonkeyTrue toMonkeyFalse

parseMonkeys :: Parser [Monkey]
parseMonkeys = parseMonkey `sepBy` newline

parseInput :: String -> Either String [Monkey]
parseInput s = case parse parseMonkeys "monkeys" s of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
data World = World
  { _monkeys :: M.Map MonkeyId Monkey,
    _numInspect :: M.Map MonkeyId Integer
  }
  deriving (Show)

makeLenses ''World

exprToInteger :: Item -> OperationExpr -> Integer
exprToInteger item OldExpr = item
exprToInteger _ (ScalarExpr v) = v

inspectItem :: Operation -> Item -> Item
inspectItem (OperationMult expr1 expr2) worry = exprToInteger worry expr1 * exprToInteger worry expr2
inspectItem (OperationAdd expr1 expr2) worry = exprToInteger worry expr1 + exprToInteger worry expr2

stepItem :: MonkeyId -> World -> World
stepItem mId w = (numInspect . at mId) %~ fmap (+ 1) $ set monkeys m'' w
  where
    m = view monkeys w
    monkey = fromJust $ M.lookup mId m
    i = head $ view heldItems monkey
    operator = view operation monkey
    i' = inspectItem operator i `div` 3
    isDiv = i' `mod` view testDivisor monkey == 0
    toMonkey = if isDiv then view ifTrue monkey else view ifFalse monkey
    m' = M.update (return . over heldItems (drop 1)) mId m
    m'' = M.update (return . over heldItems (\xs -> xs <> [i'])) toMonkey m'

step :: World -> MonkeyId -> World
step w mId = iterate (stepItem mId) w !! numItems
  where
    m = view monkeys w
    monkey = fromJust $ M.lookup mId m
    numItems = length . view heldItems $ monkey

stepRound :: World -> World
stepRound w = foldl step w [0 .. M.size (view monkeys w) - 1]

stepItem2 :: Integer -> MonkeyId -> World -> World
stepItem2 n mId w = (numInspect . at mId) %~ fmap (+ 1) $ set monkeys m'' w
  where
    m = view monkeys w
    monkey = fromJust $ M.lookup mId m
    i = head $ view heldItems monkey
    operator = view operation monkey
    i' = inspectItem operator i `mod` n
    isDiv = i' `mod` view testDivisor monkey == 0
    toMonkey = if isDiv then view ifTrue monkey else view ifFalse monkey
    m' = M.update (return . over heldItems (drop 1)) mId m
    m'' = M.update (return . over heldItems (\xs -> xs <> [i'])) toMonkey m'

step2 :: Integer -> World -> MonkeyId -> World
step2 n w mId = iterate (stepItem2 n mId) w !! numItems
  where
    m = view monkeys w
    monkey = fromJust $ M.lookup mId m
    numItems = length . view heldItems $ monkey

stepRound2 :: Integer -> World -> World
stepRound2 n w = foldl (step2 n) w [0 .. M.size (view monkeys w) - 1]

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right monkeys) = parseInput input
      mIds = view monkeyId <$> monkeys
      monkeysMap = M.fromList $ zip mIds monkeys
      initNumInspect = foldl (\m i -> M.insert i 0 m) M.empty mIds
      world = World monkeysMap initNumInspect
      endState = iterate stepRound world !! 20
      getMonkeyBusiness = product . take 2 . reverse . sort . fmap snd . M.toList . view numInspect
  print $ getMonkeyBusiness endState

  let n = foldl1 lcm $ view testDivisor <$> monkeys
      endState2 = iterate (stepRound2 n) world !! 10000
  print $ getMonkeyBusiness endState2
