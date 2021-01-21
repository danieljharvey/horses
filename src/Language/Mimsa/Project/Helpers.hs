{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Project.Helpers
  ( fromItem,
    fromType,
    fromUnitTest,
    fromStoreExpression,
    lookupExprHash,
    bindingsToVersioned,
    typeBindingsToVersioned,
    projectFromSaved,
    projectToSaved,
    getCurrentBindings,
    getCurrentTypeBindings,
    getItemsForAllVersions,
    getDependencyHashes,
    lookupBindingName,
    getBindingNames,
  )
where

import Data.Coerce
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

----------

projectFromSaved :: Store a -> SaveProject -> Project a
projectFromSaved store' sp =
  Project
    { prjStore = store',
      prjBindings = projectBindings sp,
      prjTypeBindings = projectTypes sp,
      prjUnitTests = projectUnitTests sp
    }

projectToSaved :: Project a -> SaveProject
projectToSaved proj =
  SaveProject
    { projectVersion = 1,
      projectBindings = prjBindings proj,
      projectTypes = prjTypeBindings proj,
      projectUnitTests = prjUnitTests proj
    }

fromStoreExpression :: StoreExpression ann -> ExprHash -> Project ann
fromStoreExpression storeExpr exprHash =
  mempty {prjStore = Store $ M.singleton exprHash storeExpr}

fromItem :: Name -> StoreExpression ann -> ExprHash -> Project ann
fromItem name expr hash =
  mempty
    { prjStore = Store $ M.singleton hash expr,
      prjBindings = VersionedMap $ M.singleton name (pure hash),
      prjTypeBindings = VersionedMap $ M.fromList typeList
    }
  where
    typeConsUsed =
      extractTypeDecl (storeExpression expr)
    typeList =
      (,pure hash) <$> S.toList typeConsUsed

fromType :: StoreExpression ann -> ExprHash -> Project ann
fromType expr hash =
  mempty
    { prjStore = Store $ M.singleton hash expr,
      prjTypeBindings = VersionedMap $ M.fromList typeList
    }
  where
    typeConsUsed =
      extractTypeDecl (storeExpression expr)
    typeList =
      (,pure hash) <$> S.toList typeConsUsed

fromUnitTest :: UnitTest -> StoreExpression ann -> Project ann
fromUnitTest test storeExpr =
  mempty
    { prjUnitTests = M.singleton (utExprHash test) test,
      prjStore = Store $ M.singleton (getStoreExpressionHash storeExpr) storeExpr
    }

lookupExprHash :: Project ann -> ExprHash -> Maybe (StoreExpression ann)
lookupExprHash project exprHash' =
  M.lookup exprHash' (getStore . prjStore $ project)

getBindingNames :: Project ann -> Set Name
getBindingNames =
  S.fromList . M.keys . getBindings . getCurrentBindings . prjBindings

lookupBindingName :: Project ann -> Name -> Maybe ExprHash
lookupBindingName project name =
  let b = getBindings . getCurrentBindings . prjBindings $ project
   in M.lookup name b

bindingsToVersioned :: Bindings -> VersionedBindings
bindingsToVersioned (Bindings b) = VersionedMap (pure <$> b)

typeBindingsToVersioned :: TypeBindings -> VersionedTypeBindings
typeBindingsToVersioned (TypeBindings b) = VersionedMap (pure <$> b)

getCurrentBindings :: VersionedBindings -> Bindings
getCurrentBindings versioned =
  Bindings (NE.last <$> getVersionedMap versioned)

getCurrentTypeBindings :: VersionedTypeBindings -> TypeBindings
getCurrentTypeBindings versioned =
  TypeBindings (NE.last <$> getVersionedMap versioned)

getItemsForAllVersions :: (Ord a) => VersionedMap k a -> Set a
getItemsForAllVersions versioned =
  mconcat $ M.elems (S.fromList . NE.toList <$> coerce versioned)

getDependencyHashes :: StoreExpression ann -> Set ExprHash
getDependencyHashes =
  S.fromList . M.elems . getBindings . storeBindings
