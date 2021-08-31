module Language.Mimsa.Typechecker.NormaliseTypes (normaliseType) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

data NormaliseState = NormaliseState
  { _nsNext :: Int,
    _nsAllocated :: Map TypeIdentifier Int
  }

normaliseType :: MonoType -> MonoType
normaliseType mt =
  evalState
    (normaliseType' mt)
    (NormaliseState 1 mempty)

findVar :: TypeIdentifier -> State NormaliseState Int
findVar i = do
  (NormaliseState next alloc) <- get
  case M.lookup i alloc of
    Just a -> pure a
    Nothing -> do
      put (NormaliseState (next + 1) (alloc <> M.singleton i next))
      pure next

normaliseType' :: MonoType -> State NormaliseState MonoType
normaliseType' mt = case mt of
  MTVar ann tyIdent -> do
    index <- findVar tyIdent
    pure $ MTVar ann (TVNum index)
  MTPrim ann a -> pure (MTPrim ann a)
  MTFunction ann arg fun ->
    MTFunction ann
      <$> normaliseType' arg
      <*> normaliseType' fun
  MTPair ann a b ->
    MTPair ann
      <$> normaliseType' a <*> normaliseType' b
  MTRecord ann as ->
    MTRecord ann <$> traverse normaliseType' as
  MTRecordRow ann as rest ->
    MTRecordRow ann <$> traverse normaliseType' as
      <*> normaliseType' rest
  MTArray ann a -> MTArray ann <$> normaliseType' a
  MTData ann name mts ->
    MTData ann name <$> traverse normaliseType' mts
  MTConstructor ann name ->
    pure (MTConstructor ann name)
