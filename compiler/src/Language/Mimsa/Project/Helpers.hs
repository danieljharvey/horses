{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Project.Helpers
  ( fromItem,
    fromType,
    fromTest,
    fromStoreExpression,
    fromStoreExpressionDeps,
    fromStore,
    findBindingNameForExprHash,
    findTypeBindingNameForExprHash,
    lookupExprHash,
    lookupExprHashFromStore,
    bindingsToVersioned,
    typeBindingsToVersioned,
    projectFromSaved,
    projectToSaved,
    getCurrentBindings,
    getCurrentTypeBindings,
    getItemsForAllVersions,
    getDependencyHashes,
    lookupBindingName,
    lookupTypeBindingName,
    getBindingNames,
    removeBinding,
  )
where

import Data.Coerce
import Data.Either
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Store.Storage
import Language.Mimsa.Tests.Types
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
      prjTests =
        (UTest <$> projectUnitTests sp)
          <> (PTest <$> projectPropertyTests sp)
    }

projectToSaved :: Project a -> SaveProject
projectToSaved proj =
  let (uts, pts) =
        partitionEithers $
          ( \(hash, test) -> case test of
              UTest ut -> Left (hash, ut)
              PTest pt -> Right (hash, pt)
          )
            <$> M.toList (prjTests proj)
   in SaveProject
        { projectVersion = 1,
          projectBindings = prjBindings proj,
          projectTypes = prjTypeBindings proj,
          projectUnitTests = M.fromList uts,
          projectPropertyTests = M.fromList pts
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

fromTest :: Test -> StoreExpression ann -> Project ann
fromTest = \case
  PTest pt -> fromPropertyTest pt
  UTest ut -> fromUnitTest ut

fromUnitTest :: UnitTest -> StoreExpression ann -> Project ann
fromUnitTest test storeExpr =
  mempty
    { prjTests = M.singleton (utExprHash test) (UTest test),
      prjStore = Store $ M.singleton (getStoreExpressionHash storeExpr) storeExpr
    }

fromPropertyTest :: PropertyTest -> StoreExpression ann -> Project ann
fromPropertyTest test storeExpr =
  mempty
    { prjTests = M.singleton (ptExprHash test) (PTest test),
      prjStore = Store $ M.singleton (getStoreExpressionHash storeExpr) storeExpr
    }

fromStore :: Store ann -> Project ann
fromStore store' = mempty {prjStore = store'}

-- | create a project where all the bindings of a store expression are
-- available in global scope
fromStoreExpressionDeps :: StoreExpression ann -> Project ann
fromStoreExpressionDeps se =
  mempty
    { prjBindings = bindingsToVersioned (storeBindings se),
      prjTypeBindings = typeBindingsToVersioned (storeTypeBindings se)
    }

lookupExprHash :: Project ann -> ExprHash -> Maybe (StoreExpression ann)
lookupExprHash project =
  lookupExprHashFromStore (prjStore project)

lookupExprHashFromStore :: Store ann -> ExprHash -> Maybe (StoreExpression ann)
lookupExprHashFromStore store exprHash' =
  M.lookup exprHash' (getStore store)

getBindingNames :: Project ann -> Set Name
getBindingNames =
  S.fromList . M.keys . getBindings . getCurrentBindings . prjBindings

lookupBindingName :: Project ann -> Name -> Maybe ExprHash
lookupBindingName project name =
  let b = getBindings . getCurrentBindings . prjBindings $ project
   in M.lookup name b

lookupTypeBindingName :: Project ann -> TyCon -> Maybe ExprHash
lookupTypeBindingName project tyCon =
  let b = getTypeBindings . getCurrentTypeBindings . prjTypeBindings $ project
   in M.lookup tyCon b

findBindingNameForExprHash ::
  ExprHash ->
  Project ann ->
  Map Name ExprHash
findBindingNameForExprHash exprHash project =
  let b = getBindings . getCurrentBindings . prjBindings $ project
   in M.filter (== exprHash) b

findTypeBindingNameForExprHash ::
  ExprHash ->
  Project ann ->
  Map TyCon ExprHash
findTypeBindingNameForExprHash exprHash project =
  let b = getTypeBindings . getCurrentTypeBindings . prjTypeBindings $ project
   in M.filter (== exprHash) b

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
    <> S.fromList . M.elems . getTypeBindings . storeTypeBindings

removeBinding :: Project ann -> Name -> Project ann
removeBinding prj name =
  let newBindings = delete name (prjBindings prj)
   in prj {prjBindings = newBindings}
