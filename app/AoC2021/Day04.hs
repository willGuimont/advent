module AoC2021.Day04 where

import Control.Monad.Except
import qualified Control.Monad.State as S
import qualified Data.List as L
import Text.ParserCombinators.Parsec

-- Types
type Draws = [Int]

type BoardLine = [Int]

type Board = [BoardLine]

newtype BingoState = BingoState [[Bool]] deriving (Show)

instance Semigroup BingoState where
  (BingoState x) <> (BingoState y) = BingoState [[at x i j || at y i j | i <- [0 .. 4]] | j <- [0 .. 4]]
    where
      at s i j = (s !! j) !! i

instance Monoid BingoState where
  mempty = BingoState $ replicate 5 (replicate 5 False)

-- Parsing
parseDraws :: Parser Draws
parseDraws = map read <$> many1 digit `sepBy` char ','

parseBoardLine :: Parser BoardLine
parseBoardLine = map read <$> (optional (char ' ') *> many1 digit `sepBy` many1 (char ' '))

parseBoard :: Parser Board
parseBoard = count 5 (parseBoardLine <* optional (char '\n'))

parseBingo :: Parser (Draws, [Board])
parseBingo = do
  draws <- parseDraws
  _ <- many1 (char '\n')
  boards <- parseBoard `sepBy` char '\n'
  return (draws, boards)

parseInput :: String -> Either String (Draws, [Board])
parseInput input = case parse parseBingo "bingo" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Execution
hasLineComplete :: [[Bool]] -> Bool
hasLineComplete = any and

hasBoardWon :: BingoState -> Bool
hasBoardWon (BingoState b) = hasLineComplete b || (hasLineComplete . L.transpose) b

winBingo :: Draws -> [Board] -> S.State [BingoState] (Board, BingoState, Int)
winBingo [] _ = undefined
winBingo (d : ds) bs = do
  s <- S.get
  let eq = map (BingoState . map (map (== d))) bs
      newStates = zipWith (<>) eq s
      winningBoards = filter (hasBoardWon . snd) $ zip bs newStates
   in if null winningBoards
        then S.put newStates >> winBingo ds bs
        else let (b, ns) = head winningBoards in return (b, ns, d)

getBoardScore :: (Board, BingoState, Int) -> Int
getBoardScore (b, BingoState s, d) = d * sum unmarked
  where
    bFlat = concat b
    sFlat = concat s
    unmarked = map fst $ filter (not . snd) $ zip bFlat sFlat

ordWins :: Bool -> Ordering
ordWins False = GT
ordWins True = LT

looseBingo :: Draws -> [Board] -> S.State [BingoState] (Board, BingoState, Int)
looseBingo [] _ = undefined
looseBingo (d : ds) bs = do
  s <- S.get
  let eq = map (BingoState . map (map (== d))) bs
      newStates = zipWith (<>) eq s
      newBoards = zip bs newStates
      winningBoards = filter (hasBoardWon . snd) newBoards
      loosingBoards = filter (not . hasBoardWon . snd) newBoards
   in if null loosingBoards
        then let (b, ns) = head winningBoards in return (b, ns, d)
        else S.put (snd <$> loosingBoards) >> looseBingo ds (fst <$> loosingBoards)

getAnswer :: Draws -> [Board] -> (Draws -> [Board] -> S.State [BingoState] (Board, BingoState, Int)) -> Int
getAnswer ds bs f = getBoardScore $ S.evalState (f ds bs) startState
  where
    numBoard = length bs
    startState = replicate numBoard mempty

main :: IO ()
main = do
  input <- readFile "input.txt"
  let Right (draws, boards) = parseInput input
   in do
        print $ getAnswer draws boards winBingo
        print $ getAnswer draws boards looseBingo
