module AoC2024.Day10 (partOne, partTwo) where

import Control.Monad.State qualified as S
import Data.Array (Array, array, bounds, indices, (!))
import Data.List (singleton)
import Data.Set (Set, elems, empty, insert, member)
import Text.ParserCombinators.Parsec
import Debug.Trace (trace)

-- Parsing
number :: Parser Int
-- number = read . singleton <$> digit
number = (read . singleton <$> digit) <|> (char '.' >> return (-1))

mapRow :: Parser [Int]
mapRow = many1 number

mapComplete :: Parser [[Int]]
mapComplete = mapRow `sepBy` newline

parseInput :: String -> Either String [[Int]]
parseInput s = case parse mapComplete "map" s of
  Left err -> Left $ show err
  Right res -> Right res

-- Logic
data BFSState = BFSState
  { bfsStack :: [(Int, Int)],
    bfsVisited :: Set (Int, Int)
  }

bfs :: Array (Int, Int) Int -> (Int, Int) -> [Int]
bfs arr pos = [arr ! idx | idx <- elems visited]
  where
    initState = BFSState [pos] empty
    visited = bfsVisited $ S.execState loop initState

    loop :: S.State BFSState ()
    loop = do
      BFSState {bfsStack = s, bfsVisited = v} <- S.get
      case s of
        [] -> return ()
        (p : ps) -> do
          let v' = insert p v
          S.put (BFSState ps v')
          if p `member` v
            then loop
            else do
              let ns = neighbors arr p
              let s' = ns <> ps
              S.put (BFSState s' v') >> loop

neighbors :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
neighbors arr p@(x, y) =
  [ p'
    | dx <- [-1, 0, 1],
      let x' = x + dx,
      dy <- [-1, 0, 1],
      let y' = y + dy,
      abs dx + abs dy == 1,
      x' > 0,
      y' > 0,
      x' <= n,
      y' <= m,
      let p' = (x', y'),
      p /= p',
      arr ! p' == arr ! p + 1
  ]
  where
    (_, (n, m)) = bounds arr

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right xs) = parseInput $ init input
  let n = length xs
  let m = length . head $ xs
  let arr = array ((1, 1), (n, m)) [((i, j), xs !! (i - 1) !! (j - 1)) | i <- [1 .. n], j <- [1 .. m]]
  let zeroIndices = [idx | idx <- indices arr, arr ! idx == 0]
  let result = bfs arr <$> zeroIndices
  print result
  print . sum $ length . filter (== 9) <$> result

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"
