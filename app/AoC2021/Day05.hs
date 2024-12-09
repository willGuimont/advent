module AoC2021.Day05 where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State qualified as S
import Data.Either (fromRight)
import Data.List qualified as L
import Text.ParserCombinators.Parsec

-- Types
type WorldMap = [[Int]]

type Point = (Int, Int)

type CloudLine = (Point, Point)

type Clouds = [CloudLine]

-- Parsing
parsePoint :: Parser Point
parsePoint = do
  x <- many1 digit
  _ <- char ','
  y <- many1 digit
  return (read x, read y)

parseCloudLine :: Parser CloudLine
parseCloudLine = do
  p1 <- parsePoint
  _ <- string " -> "
  p2 <- parsePoint
  return (p1, p2)

parseClouds :: Parser Clouds
parseClouds = parseCloudLine `sepBy` char '\n'

parseInput :: String -> Either String Clouds
parseInput input = case parse parseClouds "clouds" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Execution
getWorldSize :: Clouds -> (Int, Int)
getWorldSize c = (x + 1, y + 1)
  where
    flatten [] = []
    flatten ((p1, p2) : xs) = [p1, p2] <> flatten xs
    flattened = flatten c
    x = maximum $ map fst flattened
    y = maximum $ map snd flattened

makeWorld :: Clouds -> WorldMap
makeWorld c = replicate x $ replicate y 0
  where
    (x, y) = getWorldSize c

genNoDiag :: CloudLine -> [Point]
genNoDiag cl =
  let ((x, y), (x', y')) = cl
      minX = min x x'
      minY = min y y'
      maxX = max x x'
      maxY = max y y'
   in case (minX == maxX, minY == maxY) of
        (True, False) -> [(x, j) | j <- [minY .. maxY]]
        (False, True) -> [(i, y) | i <- [minX .. maxX]]
        _ -> []

genWithDiag :: CloudLine -> [Point]
genWithDiag cl =
  let ((x, y), (x', y')) = cl
      minX = min x x'
      minY = min y y'
      maxX = max x x'
      maxY = max y y'
      (sX, sY) = if x < x' then (x, y) else (x', y')
      (eX, eY) = if x < x' then (x', y') else (x, y)
      sign = if sY < eY then 1 else -1
   in case (minX == maxX, minY == maxY) of
        (True, False) -> [(x, j) | j <- [minY .. maxY]]
        (False, True) -> [(i, y) | i <- [minX .. maxX]]
        _ -> [(sX + i, sY + i * sign) | i <- [0 .. eX - sX]]

writeCloudLine :: (CloudLine -> [Point]) -> WorldMap -> CloudLine -> S.State WorldMap WorldMap
writeCloudLine genPoints m cl = do
  let points = genPoints cl
   in foldM update m points
  where
    update :: WorldMap -> (Int, Int) -> S.State WorldMap WorldMap
    update s (i, j) = S.put (over (ix i . ix j) (+ 1) s) >> S.get

writeClouds :: (CloudLine -> [Point]) -> Clouds -> S.State WorldMap WorldMap
writeClouds genPoints c = do
  m <- S.get
  foldM (writeCloudLine genPoints) m c

-- Main
showWorldMap :: WorldMap -> String
showWorldMap wMap = L.intercalate "\n" $ map (concatMap ((\x -> if x == "0" then "." else x) . show)) $ L.transpose wMap

main :: IO ()
main = do
  input <- readFile "input.txt"
  let rawClouds = parseInput input
      clouds = fromRight [] rawClouds
      initialWorld = makeWorld clouds
      (_, noDiagResult) = S.runState (writeClouds genNoDiag clouds) initialWorld
      (_, result) = S.runState (writeClouds genWithDiag clouds) initialWorld
   in do
        putStrLn "Part 1"
        putStrLn $ showWorldMap noDiagResult
        print $ length $ concatMap (filter (> 1)) noDiagResult
        putStr "\n"

        putStrLn "Part 2"
        putStrLn $ showWorldMap result
        print $ length $ concatMap (filter (> 1)) result
        putStr "\n"
