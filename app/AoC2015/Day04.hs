module AoC2015.Day04 (main) where

import Crypto.Hash
import Data.ByteString.Lazy qualified as BSL
import Data.String (fromString)

mineHash :: Int -> BSL.ByteString -> BSL.ByteString -> Int
mineHash nonce key prefix = if prefix `BSL.isPrefixOf` digest then nonce else mineHash (nonce + 1) key prefix
  where
    digest = fromString . show $ (hashlazy (BSL.concat [key, fromString (show nonce)]) :: Digest MD5)

main :: IO ()
main = do
  let key = fromString "iwrupvqb"
  print $ mineHash 0 key (fromString "00000")
  print $ mineHash 0 key (fromString "000000")
