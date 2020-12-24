module Language.Mimsa.Typechecker.NormaliseTypes (normaliseType) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

data NormaliseState
  = NormaliseState
      { _nsNext :: Int,
        _nsAllocated :: Map Int Int
      }

normaliseType :: MonoType -> MonoType
normaliseType mt =
  evalState
    (normaliseType' mt)
    (NormaliseState 1 mempty)

findVar :: Int -> State NormaliseState Int
findVar i = do
  (NormaliseState next alloc) <- get
  case M.lookup i alloc of
    Just a -> pure a
    Nothing -> do
      put (NormaliseState (next + 1) (alloc <> M.singleton i next))
      pure next

normaliseType' :: MonoType -> State NormaliseState MonoType
normaliseType' mt = case mt of
  MTVar ann (NumberedVar i) -> do
    index <- findVar i
    pure $ MTVar ann (NumberedVar index)
  MTVar ann (NamedVar n) -> pure (MTVar ann (NamedVar n))
  MTPrim ann a -> pure (MTPrim ann a)
  MTFunction ann arg fun ->
    MTFunction ann
      <$> normaliseType' arg
      <*> normaliseType' fun
  MTPair ann a b ->
    MTPair ann
      <$> normaliseType' a <*> normaliseType' b
  MTRecord ann as -> MTRecord ann <$> traverse normaliseType' as
  MTData ann name mts ->
    MTData ann name <$> traverse normaliseType' mts
