{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Project.Helpers
  ( fromItem,
    fromType,
    lookupExprHash,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

----------

fromItem :: Name -> StoreExpression ann -> ExprHash -> Project ann
fromItem name expr hash =
  Project
    { store = Store $ M.singleton hash expr,
      bindings = VersionedMap $ M.singleton name (pure hash),
      serverUrl = mempty,
      typeBindings = VersionedMap $ M.fromList typeList
    }
  where
    typeConsUsed =
      extractTypeDecl (storeExpression expr)
    typeList =
      (,pure hash) <$> S.toList typeConsUsed

fromType :: StoreExpression ann -> ExprHash -> Project ann
fromType expr hash =
  Project
    { store = Store $ M.singleton hash expr,
      bindings = mempty,
      serverUrl = mempty,
      typeBindings = VersionedMap $ M.fromList typeList
    }
  where
    typeConsUsed =
      extractTypeDecl (storeExpression expr)
    typeList =
      (,pure hash) <$> S.toList typeConsUsed

lookupExprHash :: Project ann -> ExprHash -> Maybe (StoreExpression ann)
lookupExprHash project exprHash' =
  M.lookup exprHash' (getStore . store $ project)
