{-# LANGUAGE LambdaCase #-}

module Helpers
  ( neZipWithM,
    neUnzip,
    mapInd,
    traverseInd,
    traverseIndNe,
    mapFind,
    mapToNumbered,
    tryError,
    fromRight,
  )
where

import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

neZipWithM ::
  (Applicative m) =>
  (a -> b -> m c) ->
  NE.NonEmpty a ->
  NE.NonEmpty b ->
  m (NE.NonEmpty c)
neZipWithM f as bs =
  NE.fromList <$> zipWithM f (NE.toList as) (NE.toList bs)

neUnzip :: NE.NonEmpty (a, b) -> (NE.NonEmpty a, NE.NonEmpty b)
neUnzip = bimap NE.fromList NE.fromList . unzip . NE.toList

mapInd :: (a -> Integer -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0 ..]

traverseInd ::
  (Applicative m) =>
  (a -> Integer -> m b) ->
  [a] ->
  m [b]
traverseInd f l = zipWithM f l [0 ..]

traverseIndNe ::
  (Applicative m) =>
  (a -> Integer -> m b) ->
  NE.NonEmpty a ->
  m (NE.NonEmpty b)
traverseIndNe f l = NE.fromList <$> traverseInd f (NE.toList l)

-- | find the first item that satisfies f
mapFind :: (a -> Maybe b) -> Map k a -> Maybe b
mapFind f = fmap fst . M.minView . M.mapMaybe f

mapToNumbered :: (Ord k) => Map k a -> Map k Integer
mapToNumbered =
  M.fromList
    . (\as -> zip (fmap fst as) [0 ..])
    . M.toList

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
-- coming in mtl 2.3
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

fromRight :: (Show e) => Either e a -> a
fromRight = \case
  Right a -> a
  Left e -> error (show e)
