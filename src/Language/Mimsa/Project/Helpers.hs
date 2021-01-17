{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Project.Helpers
  ( fromItem,
    fromType,
    fromUnitTest,
    lookupExprHash,
    projectFromSaved,
    projectToSaved,
    getCurrentBindings,
    getCurrentTypeBindings,
    getItemsForAllVersions,
    getDependencyHashes,
    lookupBindingName,
  )
where

import Data.Coerce
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

----------

-- | UnitTests are saved with `ann` of ()
generaliseUnitTests :: (Monoid b) => [UnitTest a] -> [UnitTest b]
generaliseUnitTests as = ($> mempty) <$> as

projectFromSaved :: (Monoid a) => Store a -> SaveProject -> Project a
projectFromSaved store' sp =
  Project
    { store = store',
      bindings = projectBindings sp,
      typeBindings = projectTypes sp,
      prjUnitTests = generaliseUnitTests (projectUnitTests sp)
    }

projectToSaved :: Project a -> SaveProject
projectToSaved proj =
  SaveProject
    { projectVersion = 1,
      projectBindings = bindings proj,
      projectTypes = typeBindings proj,
      projectUnitTests = generaliseUnitTests (prjUnitTests proj)
    }

fromItem :: Name -> StoreExpression ann -> ExprHash -> Project ann
fromItem name expr hash =
  mempty
    { store = Store $ M.singleton hash expr,
      bindings = VersionedMap $ M.singleton name (pure hash),
      typeBindings = VersionedMap $ M.fromList typeList
    }
  where
    typeConsUsed =
      extractTypeDecl (storeExpression expr)
    typeList =
      (,pure hash) <$> S.toList typeConsUsed

fromType :: StoreExpression ann -> ExprHash -> Project ann
fromType expr hash =
  mempty
    { store = Store $ M.singleton hash expr,
      typeBindings = VersionedMap $ M.fromList typeList
    }
  where
    typeConsUsed =
      extractTypeDecl (storeExpression expr)
    typeList =
      (,pure hash) <$> S.toList typeConsUsed

fromUnitTest :: UnitTest ann -> Project ann
fromUnitTest test =
  mempty {prjUnitTests = [test]}

lookupExprHash :: Project ann -> ExprHash -> Maybe (StoreExpression ann)
lookupExprHash project exprHash' =
  M.lookup exprHash' (getStore . store $ project)

lookupBindingName :: Project ann -> Name -> Maybe ExprHash
lookupBindingName project name =
  let b = getBindings . getCurrentBindings . bindings $ project
   in M.lookup name b

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
