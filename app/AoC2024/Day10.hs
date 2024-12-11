module AoC2024.Day10 (partOne, partTwo) where

import Control.Monad.State qualified as S
import Data.Array (Array, Ix, array, bounds, indices, (!))
import Data.List (singleton)
import Data.Set (Set, elems, empty, insert, member)
import DefaultMap qualified as DM
import Text.ParserCombinators.Parsec

-- Parsing
number :: Parser Int
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
    bfsVisited :: Set (Int, Int),
    bfsPaths :: DM.DefaultMap (Int, Int) Int
  }

bfs :: Bool -> Array (Int, Int) Int -> (Int, Int) -> BFSState
bfs singleVisit arr pos = S.execState loop initState
  where
    initState = BFSState [pos] empty (DM.empty 0)

    loop :: S.State BFSState ()
    loop = do
      BFSState {bfsStack = s, bfsVisited = v, bfsPaths = paths} <- S.get
      case s of
        [] -> return ()
        (p : ps) -> do
          let paths' = DM.adjust (+ 1) p paths
          let v' = insert p v
          S.put (BFSState ps v' paths')
          if singleVisit
            then do
              if p `member` v
                then loop
                else do
                  let ns = neighbors arr p
                  let s' = ns <> ps
                  S.put (BFSState s' v' paths') >> loop
            else do
              let ns = neighbors arr p
              let s' = ns <> ps
              S.put (BFSState s' v' paths') >> loop

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

getAtIndices :: (Ix i) => Array i a -> [i] -> [a]
getAtIndices arr xs = [arr ! x | x <- xs]

indicesOf :: Array (Int, Int) Int -> Int -> [(Int, Int)]
indicesOf arr val = [idx | idx <- indices arr, arr ! idx == val]

countPaths :: Array (Int, Int) Int -> DM.DefaultMap (Int, Int) Int -> Int
countPaths arr dm = sum $ (`DM.lookup` dm) <$> nines
  where
    nines = indicesOf arr 9

-- Parts
partOne :: String -> IO ()
partOne input = do
  putStrLn "- Part One -"

  let (Right xs) = parseInput $ init input

  let n = length xs
  let m = length . head $ xs
  let arr = array ((1, 1), (n, m)) [((i, j), xs !! (i - 1) !! (j - 1)) | i <- [1 .. n], j <- [1 .. m]]

  let zeroIndices = indicesOf arr 0
  let bfsResults = getAtIndices arr . elems . bfsVisited . bfs True arr <$> zeroIndices
  print . sum $ length . filter (== 9) <$> bfsResults

partTwo :: String -> IO ()
partTwo input = do
  putStrLn "- Part Two -"

  let (Right xs) = parseInput $ init input

  let n = length xs
  let m = length . head $ xs
  let arr = array ((1, 1), (n, m)) [((i, j), xs !! (i - 1) !! (j - 1)) | i <- [1 .. n], j <- [1 .. m]]

  let zeroIndices = [idx | idx <- indices arr, arr ! idx == 0]
  let bfsResults = bfsPaths . bfs False arr <$> zeroIndices
  print . sum $ countPaths arr <$> bfsResults
