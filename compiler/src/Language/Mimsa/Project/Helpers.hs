{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Project.Helpers
  ( fromItem,
    fromType,
    fromStoreExpression,
    fromStoreExpressionDeps,
    fromModuleDeps,
    fromStore,
    fromModuleStore,
    fromModule,
    findBindingNameForExprHash,
    findAnyBindingNameForExprHash,
    findTypeBindingNameForExprHash,
    findAnyTypeBindingNameForExprHash,
    lookupModuleHash,
    lookupExprHash,
    lookupExprHashFromStore,
    typeBindingsToVersioned,
    bindingsToVersioned,
    toVersioned,
    projectFromSaved,
    projectToSaved,
    getCurrentBindings,
    getCurrentTypeBindings,
    getCurrentModules,
    getItemsForAllVersions,
    getDependencyHashes,
    getModuleDependencyHashes,
    lookupBindingName,
    lookupTypeBindingName,
    lookupModuleName,
    getBindingNames,
    removeBinding,
  )
where

import Data.Bifunctor
import Data.Coerce
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

----------

projectFromSaved ::
  Map ModuleHash (Module ann) ->
  Store ann ->
  SaveProject ->
  Project ann
projectFromSaved moduleStore store' sp =
  Project
    { prjStore = store',
      prjBindings = projectBindings sp,
      prjTypeBindings = projectTypes sp,
      prjModules = projectModules sp,
      prjModuleStore = moduleStore
    }

projectToSaved :: Project a -> SaveProject
projectToSaved proj =
  SaveProject
    { projectVersion = 1,
      projectBindings = prjBindings proj,
      projectTypes = prjTypeBindings proj,
      projectModules = prjModules proj
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

fromModule :: ModuleName -> Module ann -> Project ann
fromModule modName newModule =
  let (_, moduleHash) = serializeModule newModule
   in mempty
        { prjModules = VersionedMap $ M.singleton modName (pure moduleHash),
          prjModuleStore = M.singleton moduleHash newModule
        }

fromStore :: Store ann -> Project ann
fromStore store' = mempty {prjStore = store'}

fromModuleStore :: Map ModuleHash (Module ann) -> Project ann
fromModuleStore modules = mempty {prjModuleStore = modules}

removeNamespaceFromKey :: (Ord k2) => Map (k1, k2) a -> Map k2 a
removeNamespaceFromKey = M.fromList . fmap (first snd) . M.toList

-- | create a project where all the bindings of a store expression are
-- available in global scope
fromStoreExpressionDeps :: StoreExpression ann -> Project ann
fromStoreExpressionDeps se =
  mempty
    { prjBindings = toVersioned (removeNamespaceFromKey $ storeBindings se),
      prjTypeBindings = toVersioned (removeNamespaceFromKey $ storeTypeBindings se)
    }

-- | create a project where all the bindings of a store expression are
-- available in global scope
fromModuleDeps :: Map ModuleHash (Module ann) -> Module ann -> Project ann
fromModuleDeps _moduleStore _mod' =
  mempty
    { prjModules = mempty
    }

lookupExprHash :: Project ann -> ExprHash -> Maybe (StoreExpression ann)
lookupExprHash project =
  lookupExprHashFromStore (prjStore project)

lookupExprHashFromStore :: Store ann -> ExprHash -> Maybe (StoreExpression ann)
lookupExprHashFromStore store exprHash' =
  M.lookup exprHash' (getStore store)

lookupModuleHash :: Project ann -> ModuleHash -> Maybe (Module ann)
lookupModuleHash project modHash =
  M.lookup modHash (prjModuleStore project)

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

lookupModuleName :: Project ann -> ModuleName -> Either (Set ModuleName) ModuleHash
lookupModuleName project modName =
  let b = getCurrentModules . prjModules $ project
   in case M.lookup modName b of
        Just a -> pure a
        _ -> Left (M.keysSet b)

-- | find the binding name in current expr hashes
findBindingNameForExprHash ::
  ExprHash ->
  Project ann ->
  Map Name ExprHash
findBindingNameForExprHash exprHash project =
  let b = getBindings . getCurrentBindings . prjBindings $ project
   in M.filter (== exprHash) b

-- | find the binding name in expr hashes
findAnyBindingNameForExprHash ::
  ExprHash ->
  Project ann ->
  Maybe Name
findAnyBindingNameForExprHash exprHash project =
  let bindings = M.toList $ getVersionedMap (prjBindings project)
      predicate (_, hashes) = elem exprHash (NE.toList hashes)
   in case filter predicate bindings of
        [(name, _)] -> Just name
        _ -> Nothing

-- | find the type binding name in current expr hashes
findTypeBindingNameForExprHash ::
  ExprHash ->
  Project ann ->
  Map TyCon ExprHash
findTypeBindingNameForExprHash exprHash project =
  let b = getTypeBindings . getCurrentTypeBindings . prjTypeBindings $ project
   in M.filter (== exprHash) b

-- | find the type binding name in expr hashes
findAnyTypeBindingNameForExprHash ::
  ExprHash ->
  Project ann ->
  Maybe TyCon
findAnyTypeBindingNameForExprHash exprHash project =
  let bindings = M.toList $ getVersionedMap (prjTypeBindings project)
      predicate (_, hashes) = elem exprHash (NE.toList hashes)
   in case filter predicate bindings of
        [(tyCon, _)] -> Just tyCon
        _ -> Nothing

toVersioned :: Map k a -> VersionedMap k a
toVersioned b = VersionedMap (pure <$> b)

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

getCurrentModules :: VersionedModules -> Map ModuleName ModuleHash
getCurrentModules = fmap NE.last . getVersionedMap

getItemsForAllVersions :: (Ord a) => VersionedMap k a -> Set a
getItemsForAllVersions versioned =
  mconcat $ M.elems (S.fromList . NE.toList <$> coerce versioned)

getDependencyHashes :: StoreExpression ann -> Set ExprHash
getDependencyHashes =
  S.fromList . M.elems . storeBindings
    <> S.fromList . M.elems . storeTypeBindings

getModuleDependencyHashes :: Module ann -> Set ModuleHash
getModuleDependencyHashes inputModule =
  S.fromList
    ( M.elems (moExpressionImports inputModule)
        <> M.elems (moNamedImports inputModule)
    )

removeBinding :: Project ann -> Name -> Project ann
removeBinding prj name =
  let newBindings = delete name (prjBindings prj)
   in prj {prjBindings = newBindings}
