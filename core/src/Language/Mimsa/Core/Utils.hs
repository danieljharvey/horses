module Language.Mimsa.Core.Utils (mapWithIndex, setMapMaybe, mapKeys, filterMapKeys, addNumbersToMap) where

import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

-- I give in, it is time to throw all these things in a file

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as =
  uncurry f <$> zip [1 ..] as

setMapMaybe :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe f = S.fromList . mapMaybe f . S.toList

mapKeys :: (Ord k2) => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys f = M.fromList . fmap (first f) . M.toList

-- useful to break apart maps where
-- key is a sum type
filterMapKeys :: (Ord k2) => (k -> Maybe k2) -> Map k a -> Map k2 a
filterMapKeys f =
  M.fromList . mapMaybe (\(k, a) -> (,) <$> f k <*> pure a) . M.toList

addNumbersToMap :: (Ord k) => Map k a -> Map k (Int, a)
addNumbersToMap =
  M.fromList
    . fmap (\(i, (k, a)) -> (k, (i, a)))
    . zip [0 ..]
    . M.toList
