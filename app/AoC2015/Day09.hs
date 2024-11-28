module AoC2015.Day09 (main) where

import Control.Lens
import Control.Monad.Except (throwError)
import qualified Control.Monad.State as S
import Data.List (permutations, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as SS
import Text.ParserCombinators.Parsec

-- Types
type LocationName = String

type LocationId = Int

type Distance = Int

type DistanceBetweenName = (LocationName, LocationName, Distance)

type DistanceBetween = (LocationId, LocationId, Distance)

-- Parsing
parseDistanceBetween :: Parser DistanceBetweenName
parseDistanceBetween = do
  from <- many1 letter
  _ <- string " to "
  to <- many1 letter
  _ <- string " = "
  distance <- read <$> many1 digit
  return (from, to, distance)

parseMap :: Parser [DistanceBetweenName]
parseMap = parseDistanceBetween `sepBy` newline

parseInput :: String -> Either String [DistanceBetweenName]
parseInput s = case parse parseMap "map" s of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
toMapId :: [DistanceBetweenName] -> [DistanceBetween]
toMapId xs =
  snd $
    foldl
      ( \(m, ds) (n1, n2, distance) ->
          let (m', i1) = addId m n1
           in let (m'', i2) = addId m' n2 in (m'', (i1, i2, distance) : ds)
      )
      (M.empty, [])
      xs
  where
    addId :: M.Map LocationName LocationId -> LocationName -> (M.Map LocationName LocationId, LocationId)
    addId m n = if n `M.member` m then (m, fromJust $ M.lookup n m) else (M.insert n (M.size m) m, M.size m)

type Graph = [[Distance]]

infinity :: Int
infinity = 9999999

makeGraph :: Int -> Graph
makeGraph n = [[0 | _ <- [1 .. n]] | _ <- [1 .. n]]

addEdge :: Graph -> DistanceBetween -> Graph
addEdge g (from, to, dist) = set (ix from . ix to) dist . set (ix to . ix from) dist $ g

fstOf3 :: (a, b, c) -> a
fstOf3 (x, _, _) = x

shortestPath :: Graph -> Int -> Int
shortestPath g firstNode =
  fstOf3 $
    foldl
      ( \(dist, i, alreadyVisited) _ ->
          let j = findNextNode alreadyVisited i in ((g !! i !! j) + dist, j, SS.insert j alreadyVisited)
      )
      (0, firstNode, SS.singleton firstNode)
      [1 .. length g - 1]
  where
    findNextNode :: SS.Set Int -> Int -> Int
    findNextNode alreadyVisited from =
      let xs = sortBy (\x y -> compare (fst x) (fst y)) (zip (g !! from) [0 ..]) :: [(Distance, Int)]
       in snd . head $ filter (\(dist, j) -> dist /= 0 && j `SS.notMember` alreadyVisited) xs

getCostOfPath :: Graph -> [Int] -> Int
getCostOfPath g (x : y : xs) = (g !! x !! y) + getCostOfPath g (y : xs)
getCostOfPath _ [_] = 0
getCostOfPath _ [] = 0

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right mStr) = parseInput input
      m = toMapId mStr
      numCities = (1 +) . maximum . fmap (\(i, j, _) -> max i j) $ m
      gInit = makeGraph numCities
      g = foldl addEdge gInit m
  print . minimum $ shortestPath g <$> [0 .. numCities - 1]
  print . maximum $ getCostOfPath g <$> permutations [0 .. numCities - 1]
