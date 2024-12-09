{-# LANGUAGE TemplateHaskell #-}

module DefaultMap
  ( DefaultMap,
    empty,
    singleton,
    fromList,
    insert,
    adjust,
    adjustWithKey,
    delete,
    lookup,
    (!),
    member,
    notMember,
    null,
    size,
    toList,
    keys,
  )
where

import Control.Lens
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe
import Prelude hiding (lookup, null)

data DefaultMap k v = DefMap {_defDefault :: v, _defMap :: M.Map k v}
  deriving (Show, Eq)

makeLenses ''DefaultMap

-- Instance
instance Functor (DefaultMap k) where
  -- Maps function on the default value and values of the map
  fmap f dm =
    DefMap {_defDefault = f (dm ^. defDefault), _defMap = f <$> dm ^. defMap}

instance (Ord k) => Applicative (DefaultMap k) where
  -- Empty map with x as a default
  pure = empty

  -- Applies default of f to the default of x
  -- Applies f[k] to x[k] for each k in the keys in x and f
  f <*> x = go (nub $ keys f ++ keys x) $ empty def
    where
      def = f ^. defDefault $ x ^. defDefault
      go (k : ks) m =
        let m' = insert k ((f ! k) (x ! k)) m
         in go ks m'
      go [] m = m

instance Foldable (DefaultMap k) where
  foldr f z m = f (m ^. defDefault) $ foldr f z m'
    where
      m' = m ^. defMap

instance (Ord k) => Traversable (DefaultMap k) where
  traverse f m = fromList <$> def <*> list
    where
      bs = traverse f $ snd <$> toList m
      list = zip (keys m) <$> bs
      def = f (m ^. defDefault)

mapOnMap :: (M.Map k v -> M.Map kk v) -> DefaultMap k v -> DefaultMap kk v
mapOnMap f m = DefMap (m ^. defDefault) $ f (m ^. defMap)

-- Construction
empty :: v -> DefaultMap k v
empty def = DefMap def M.empty

singleton :: v -> k -> v -> DefaultMap k v
singleton def k v = DefMap def $ M.singleton k v

fromList :: (Ord k) => v -> [(k, v)] -> DefaultMap k v
fromList def xs = DefMap def $ M.fromList xs

-- Insertion
insert :: (Ord k) => k -> v -> DefaultMap k v -> DefaultMap k v
insert k x = mapOnMap (M.insert k x)

-- Deletion/Update
adjust :: (Ord k) => (v -> v) -> k -> DefaultMap k v -> DefaultMap k v
adjust f = adjustWithKey (\_ y -> f y)

adjustWithKey ::
  (Ord k) => (k -> v -> v) -> k -> DefaultMap k v -> DefaultMap k v
adjustWithKey f k m = (insert k $ f k $ lookup k m) m

delete :: (Ord k) => k -> DefaultMap k v -> DefaultMap k v
delete k = mapOnMap (M.delete k)

-- Query
lookup :: (Ord k) => k -> DefaultMap k v -> v
lookup k m = fromMaybe (m ^. defDefault) $ M.lookup k (m ^. defMap)

(!) :: (Ord k) => DefaultMap k v -> k -> v
m ! k = lookup k m

member :: (Ord k) => k -> DefaultMap k v -> Bool
member k = M.member k . view defMap

notMember :: (Ord k) => k -> DefaultMap k v -> Bool
notMember k = M.notMember k . view defMap

null :: DefaultMap k v -> Bool
null m = size m == 0

size :: DefaultMap k v -> Int
size = M.size . view defMap

-- Transformation
toList :: DefaultMap k v -> [(k, v)]
toList = M.toList . view defMap

keys :: DefaultMap k v -> [k]
keys = fmap fst . toList

