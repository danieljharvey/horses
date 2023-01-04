module Language.Mimsa.Project.Helpers
  ( fromStoreExpression,
    fromModuleDeps,
    fromStore,
    fromModuleStore,
    fromModule,
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
    lookupModuleName,
  )
where

import Data.Coerce
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Core
import Language.Mimsa.Modules.HashModule
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
      prjModules = projectModules sp,
      prjModuleStore = moduleStore
    }

projectToSaved :: Project a -> SaveProject
projectToSaved proj =
  SaveProject
    { projectVersion = 1,
      projectModules = prjModules proj
    }

fromStoreExpression :: StoreExpression ann -> ExprHash -> Project ann
fromStoreExpression storeExpr exprHash =
  mempty {prjStore = Store $ M.singleton exprHash storeExpr}

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

lookupModuleName :: Project ann -> ModuleName -> Either (Set ModuleName) ModuleHash
lookupModuleName project modName =
  let b = getCurrentModules . prjModules $ project
   in case M.lookup modName b of
        Just a -> pure a
        _ -> Left (M.keysSet b)

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
