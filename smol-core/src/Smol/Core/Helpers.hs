{-# LANGUAGE LambdaCase #-}

module Smol.Core.Helpers
  ( neZipWithM,
    neUnzip,
    mapInd,
    traverseInd,
    traverseIndNe,
    mapFind,
    mapToNumbered,
    tryError,
    fromRight,
    foldMapM,
    filterMapKeys,
    mapKey,
    tracePrettyM,
  )
where

import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable (foldlM)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Debug.Trace (traceM)
import Smol.Core.Printer

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
tryError :: (MonadError e m) => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

fromRight :: (Show e) => Either e a -> a
fromRight = \case
  Right a -> a
  Left e -> error (show e)

-- useful to break apart maps where
-- key is a sum type
filterMapKeys :: (Ord k2) => (k -> Maybe k2) -> Map k a -> Map k2 a
filterMapKeys f =
  M.fromList . mapMaybe (\(k, a) -> (,) <$> f k <*> pure a) . M.toList

-------

-- from https://hackage.haskell.org/package/rio-0.1.22.0/docs/src/RIO.Prelude.Extra.html#foldMapM

-- | Extend 'foldMap' to allow side effects.
--
-- Internally, this is implemented using a strict left fold. This is used for
-- performance reasons. It also necessitates that this function has a @Monad@
-- constraint and not just an @Applicative@ constraint. For more information,
-- see
-- <https://github.com/commercialhaskell/rio/pull/99#issuecomment-394179757>.
--
-- @since 0.1.3.0
foldMapM ::
  (Monad m, Monoid w, Foldable t) =>
  (a -> m w) ->
  t a ->
  m w
foldMapM f =
  foldlM
    ( \acc a -> do
        w <- f a
        return $! mappend acc w
    )
    mempty

mapKey :: (Ord k1) => (k -> k1) -> Map k a -> Map k1 a
mapKey f = M.fromList . fmap (first f) . M.toList

tracePrettyM :: (Printer a, Monad m) => String -> a -> m ()
tracePrettyM msg a = traceM (msg <> ":" <> T.unpack (renderWithWidth 40 $ prettyDoc a))
