module AoC2023.Day03 (main) where

import Data.Array qualified as A
import Data.Char (isDigit)
import Data.Maybe (isJust)

-- Types
type Pos = (Int, Int)

type World = A.Array Pos Char

data PartNumber = PartNumber
  { value :: Int,
    nearSymbol :: Bool,
    start :: Pos,
    end :: Pos
  }
  deriving (Show)

-- Part 1
mkWorld :: String -> World
mkWorld s = A.array ((1, 1), (r, c)) [((i, j), xs !! (i - 1) !! (j - 1)) | i <- [1 .. r], j <- [1 .. c]]
  where
    xs = lines s
    r = length xs
    c = length . head $ xs

neighborIdx :: World -> Pos -> [Pos]
neighborIdx w (i, j) =
  [ (ii, jj)
    | di <- delta,
      let ii = i + di,
      dj <- delta,
      let jj = j + dj,
      di /= 0 || dj /= 0,
      (ii, jj) `elem` idx
  ]
  where
    delta = [-1, 0, 1] :: [Int]
    idx = A.indices w

isSymbol :: World -> Pos -> Bool
isSymbol w p = let c = w A.! p in not (isDigit c) && c /= '.'

hasSymbolAround :: World -> Pos -> Bool
hasSymbolAround w p = any (isSymbol w) $ neighborIdx w p

hasSymbolAroundMultiple :: World -> [Pos] -> Bool
hasSymbolAroundMultiple w = any (hasSymbolAround w)

parseNumber :: World -> Int -> Maybe (PartNumber, Int)
parseNumber w p
  | not (null valueStr) = Just (part, p + length valueStr)
  | validIdx p = parseNumber w (p + 1)
  | otherwise = Nothing
  where
    idx = A.indices w
    validIdx i = i >= 0 && i < length idx
    consecDigits :: Int -> [Pos]
    consecDigits i =
      if validIdx i && ii `elem` idx && isDigit (w A.! ii)
        then ii : consecDigits (i + 1)
        else []
      where
        ii = idx !! i
    startRow = fst $ head (consecDigits p)
    valueStr = (w A.!) <$> filter (\p' -> startRow == fst p') (consecDigits p)
    val = read valueStr
    part = PartNumber val (hasSymbolAroundMultiple w $ consecDigits p) (head $ consecDigits p) (last $ consecDigits p)

day1 :: World -> [PartNumber]
day1 w = go 0
  where
    go :: Int -> [PartNumber]
    go i = if isJust partMaybe then part : go i' else []
      where
        partMaybe = parseNumber w i
        Just (part, i') = partMaybe

-- Part 2
posBetween :: Pos -> Pos -> [Pos]
posBetween (i, j) (_, j') = [(i, j'') | j'' <- [j .. j']]

gearIndices :: World -> [Pos]
gearIndices w = filter (\p -> w A.! p == '*') $ A.indices w

neighborIdxPart :: World -> PartNumber -> [Pos]
neighborIdxPart w (PartNumber _ _ s e) = concatMap (neighborIdx w) (posBetween s e)

partsCloseTo :: World -> Pos -> [PartNumber] -> [PartNumber]
partsCloseTo w p = filter (\part -> p `elem` neighborIdxPart w part)

day2 :: World -> [PartNumber] -> Int
day2 w ps = sum multPerGear
  where
    gs = gearIndices w
    partsNearGear = partsCloseTo w <$> gs <*> [ps]
    twoPartsNearGear = filter (\x -> length x == 2) partsNearGear
    multPerGear = product . fmap value <$> twoPartsNearGear

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let world = mkWorld input
  let ps = day1 world
  print . sum $ value <$> filter near ps
  print $ day2 world ps
  where
    near (PartNumber _ b _ _) = b
