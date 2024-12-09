module AoC2022.Day12 (main) where

import Control.Lens
import Control.Monad.Extra (untilJustM, when)
import Control.Monad.State.Strict qualified as S
import Data.Char (ord)
import Data.List (elemIndex, elemIndices, foldl')
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as SS

-- Types
type LocationId = Int

type Distance = Int

type DistanceBetween = (LocationId, LocationId, Distance)

-- Logic
type Graph = [[Distance]]

infinity :: Int
infinity = 9999999

makeGraph :: Int -> Graph
makeGraph n = [[if x == y then 0 else infinity | x <- [1 .. n]] | y <- [1 .. n]]

addEdge :: Graph -> DistanceBetween -> Graph
addEdge g (fromNode, toNode, dist) = set (ix fromNode . ix toNode) dist g

toHeight :: Char -> Int
toHeight c = ord c - ord 'a'

toPos :: Int -> Int -> (Int, Int)
toPos size posFlat = (posFlat `div` size, posFlat `mod` size)

toFlat :: Int -> (Int, Int) -> Int
toFlat size (x, y) = x * size + y

findEdges :: [[Int]] -> [DistanceBetween]
findEdges m = foldl' go [] $ toPos sy <$> [0 .. totalSize - 1]
  where
    delta = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], abs x + abs y == 1]
    (sx, sy) = (length m, length . head $ m)
    totalSize = sx * sy
    inRange x y = 0 <= x && x < sx && 0 <= y && y < sy
    go edges (x, y) = foldl' (\es (dx, dy) -> let (x', y') = (x + dx, y + dy) in if inRange x' y' && (m !! x' !! y') <= (m !! x !! y) + 1 then (toFlat sy (x, y), toFlat sy (x', y'), 1) : es else es) edges delta

dijkstra :: Graph -> LocationId -> LocationId -> Int
dijkstra graph src tgt = S.evalState go ([], M.empty, M.empty, SS.empty)
  where
    numVertices = length graph
    vertices = [0 .. numVertices - 1]
    go :: S.State ([Int], M.Map Int Int, M.Map Int Int, SS.Set Int) Int
    go = do
      (q, dist, prev, visited) <- S.get
      S.put (src : q, M.insert src 0 dist, prev, SS.insert src visited)
      mapM_ initV vertices
      untilJustM step

    step :: S.State ([Int], M.Map Int Int, M.Map Int Int, SS.Set Int) (Maybe Int)
    step = do
      (q, dist, prev, visited) <- S.get
      if null q
        then return . Just $ infinity
        else
          ( do
              let (u : q') = q
                  alt = (1 +) $ fromJust $ M.lookup u dist
                  neighbors = getNeighbors u
              S.put (q', dist, prev, SS.insert u visited)
              if u == tgt
                then return $ M.lookup u dist
                else
                  ( do
                      mapM_
                        ( \n -> do
                            (q'', dist', prev', visited') <- S.get
                            let newQ = if n `SS.notMember` visited' && n `notElem` q'' then q'' <> [n] else q''
                                newDist = if n `SS.notMember` visited' then M.insert n alt dist' else dist'
                                newPrev = if n `SS.notMember` visited' then M.insert n u prev' else prev'
                            S.put (newQ, newDist, newPrev, visited')
                        )
                        neighbors
                      return Nothing
                  )
          )

    getNeighbors :: Int -> [Int]
    getNeighbors u = fmap snd . filter (\(x, _) -> x /= infinity) $ zip (graph !! u) [0 ..]

    initV :: Int -> S.State ([Int], M.Map Int Int, M.Map Int Int, SS.Set Int) ()
    initV v = do
      (q, dist, prev, visited) <- S.get
      when (v /= src) $ S.put (q, M.insert v infinity dist, M.insert v (-1) prev, visited)

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let heightMap = fmap toHeight <$> lines input
      flatMap = concat heightMap

      startPosFlat = fromJust $ elemIndex (-14) flatMap
      endPosFlat = fromJust $ elemIndex (-28) flatMap

      heightMap' = fmap (\x -> if x == (-14) then 0 else x) <$> heightMap
      heightMap'' = fmap (\x -> if x == (-28) then ord 'z' - ord 'a' else x) <$> heightMap'

      numNodes = length flatMap
      gInit = makeGraph numNodes
      edges = findEdges heightMap''
      g = foldl' addEdge gInit edges

      possibleStartingPoints = elemIndices 0 flatMap
  print $ dijkstra g startPosFlat endPosFlat
  print . minimum $ (\s -> dijkstra g s endPosFlat) <$> possibleStartingPoints
