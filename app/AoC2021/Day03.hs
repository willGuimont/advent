module AoC2021.Day03 where

import Data.List qualified as L

data Bit = Zero | One deriving (Show, Eq)

bitToBool :: Bit -> Bool
bitToBool Zero = False
bitToBool One = True

boolToBit :: Bool -> Bit
boolToBit False = Zero
boolToBit True = One

notBit :: Bit -> Bit
notBit Zero = One
notBit One = Zero

parseBit :: Char -> Bit
parseBit '0' = Zero
parseBit '1' = One
parseBit _ = undefined

countBits :: [Bit] -> (Int, Int)
countBits = foldl concatTuple (0, 0) . fmap f
  where
    f Zero = (1, 0)
    f One = (0, 1)
    concatTuple (x, y) (z, w) = (x + z, y + w)

countToBit :: (Int, Int) -> Bit
countToBit (zeroes, ones) = if ones > zeroes then One else Zero

bitsToInt :: [Bit] -> Int
bitsToInt = sum . map (2 ^) . L.elemIndices One . reverse

gamma :: [[Bit]] -> Int
gamma bits = bitsToInt counted
  where
    t = L.transpose bits
    counted = map (countToBit . countBits) t

epsilon :: [[Bit]] -> Int
epsilon = gamma . map (map notBit)

getByIndices :: [Int] -> [a] -> [a]
getByIndices idx = map snd . filter ((`elem` idx) . fst) . zip [0 ..]

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = if p x then while p f (f x) else x

lifeSupport :: (Int -> Int -> Bool) -> [[Bit]] -> Int
lifeSupport cmp xs = bitsToInt . head $ fst results
  where
    getNewCount = map ((boolToBit . uncurry cmp) . countBits) . L.transpose
    results =
      while
        ((/= 1) . length . fst)
        (\(bs, n) -> (bitsFilter (getNewCount bs) bs n, n + 1))
        (xs, 0)

oxygen :: [[Bit]] -> Int
oxygen = lifeSupport (<=)

bitsFilter :: [Bit] -> [[Bit]] -> Int -> [[Bit]]
bitsFilter bs bits n = filter (\b -> b !! n == bs !! n) bits

co2 :: [[Bit]] -> Int
co2 = lifeSupport (>)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ls = lines input
      bits = map (map parseBit) ls
   in do
        print $ gamma bits * epsilon bits
        print $ oxygen bits * co2 bits
