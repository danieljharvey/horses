module Language.Mimsa.Typechecker.NormaliseTypes (normaliseType) where

import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Language.Mimsa.Core

data NormaliseState = NormaliseState
  { _nsNext :: Int,
    _nsAllocated :: Map TypeIdentifier Int
  }

normaliseType :: (Monoid ann) => Type ann -> Type ann
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

normaliseType' :: (Monoid ann) => Type ann -> State NormaliseState (Type ann)
normaliseType' (MTVar ann tyIdent) = do
  index <- findVar tyIdent
  pure $ MTVar ann (TVUnificationVar index)
normaliseType' other = bindType normaliseType' other
