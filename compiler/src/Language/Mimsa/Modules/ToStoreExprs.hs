{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Modules.ToStoreExprs (toStoreExpressions, CompiledModule (..)) where

import Control.Monad.Except
import Control.Monad.Identity
import Data.Bifunctor
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import Language.Mimsa.Modules.Dependencies
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

data CompiledModule ann = CompiledModule
  { cmStore :: Store ann,
    cmExprs :: Map DefIdentifier ExprHash
  }
  deriving stock (Eq, Ord, Show, Functor)

instance Semigroup (CompiledModule ann) where
  (CompiledModule a b) <> (CompiledModule a' b') =
    CompiledModule (a <> a') (b <> b')

instance Monoid (CompiledModule ann) where
  mempty = CompiledModule mempty mempty

instance (Printer ann) => Printer (CompiledModule ann) where
  prettyPrint (CompiledModule store exprs) =
    prettyPrint
      ( M.fromList
          [ ("store" :: Text, prettyPrint store),
            ("exprs", prettyPrint exprs)
          ]
      )

toStoreExpressions ::
  (MonadError (Error Annotation) m, Eq ann, Monoid ann, Show ann) =>
  Map ModuleHash (Module (Type ann)) ->
  Module (Type ann) ->
  m (CompiledModule (Type ann))
toStoreExpressions typecheckedModules inputModule = do
  allCompiledModules <- compileAllModules typecheckedModules inputModule
  let (_, rootModuleHash) = serializeModule inputModule
  case M.lookup rootModuleHash allCompiledModules of
    Just (CompiledModule _ compiledMod) ->
      -- we want the compiled module for the main thing but with all the store
      -- items
      let withBigStore = mconcat (M.elems allCompiledModules)
       in pure $ withBigStore {cmExprs = compiledMod}
    Nothing -> throwError (ModuleErr $ MissingModule rootModuleHash)

-- if `b` needs `a`, and `c` needs `b`, add `a` to the deps for `c`
includeTransitiveDeps :: (Eq a, Ord k, Show k) => Map k (a, Set k) -> Map k (a, Set k)
includeTransitiveDeps depsMap = runIdentity $ do
  let state =
        Build.State
          { Build.stInputs =
              ( \(a, deps') ->
                  Build.Plan
                    { Build.jbDeps = deps',
                      Build.jbInput = a
                    }
              )
                <$> depsMap,
            Build.stOutputs = mempty
          }

  let action deps' a = do
        let allDeps = M.keysSet deps' <> foldMap snd deps'
        pure (a, allDeps)

  Build.stOutputs <$> Build.doJobs action state

--- compile many modules
compileAllModules ::
  (MonadError (Error Annotation) m, Eq ann, Monoid ann, Show ann) =>
  Map ModuleHash (Module (Type ann)) ->
  Module (Type ann) ->
  m (Map ModuleHash (CompiledModule (Type ann)))
compileAllModules myDeps rootModule = do
  -- which other modules do we need to compile in order to compile this one?
  inputWithDeps <- includeTransitiveDeps <$> getModuleDeps myDeps rootModule

  let state =
        Build.State
          { Build.stInputs =
              ( \(mod', deps) ->
                  Build.Plan
                    { Build.jbDeps = deps,
                      Build.jbInput = mod'
                    }
              )
                <$> inputWithDeps,
            Build.stOutputs = mempty
          }
  -- go!
  Build.stOutputs
    <$> Build.doJobs compileModuleDefinitions state

--- compile a module into StoreExpressions
compileModuleDefinitions ::
  ( MonadError (Error Annotation) m,
    Eq ann,
    Monoid ann
  ) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  m (CompiledModule (Type ann))
compileModuleDefinitions compiledModules inputModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <-
    getDependencies extractUsesTyped inputModule

  let inputWithDepsAndName = M.mapWithKey (,) inputWithDeps

  let state =
        Build.State
          { Build.stInputs =
              ( \(name, (expr, deps, uses)) ->
                  Build.Plan
                    { Build.jbDeps = deps,
                      Build.jbInput = (name, expr, uses)
                    }
              )
                <$> inputWithDepsAndName,
            Build.stOutputs = mempty
          }

  -- go!
  storeExprs <-
    Build.stOutputs
      <$> Build.doJobs (toStoreExpression compiledModules inputModule) state

  pure $
    CompiledModule
      { cmStore = toStore storeExprs,
        cmExprs = getStoreExpressionHash <$> storeExprs
      }

toStoreExpression ::
  ( MonadError (Error Annotation) m,
    Monoid ann
  ) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann)) ->
  (DefIdentifier, DepType (Type ann), Set Entity) ->
  m (StoreExpression (Type ann))
toStoreExpression compiledModules inputModule inputs (_, dep, uses) =
  case dep of
    (DTExpr expr) -> exprToStoreExpression compiledModules inputModule inputs (expr, uses)
    (DTData dt) -> dataTypeToStoreExpression compiledModules inputModule inputs dt

-- this is crap, need to add type bindings
dataTypeToStoreExpression ::
  ( Monoid ann,
    MonadError (Error Annotation) m
  ) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann)) ->
  DataType ->
  m (StoreExpression (Type ann))
dataTypeToStoreExpression compiledModules inputModule inputs dt = do
  let mt = MTRecord mempty mempty -- it's weird MyData needs this, so give it rubbish
      expr = MyData mt dt (MyRecord mt mempty)
      uses = extractDataTypeUses dt
  types <- typesFromEntities compiledModules inputModule inputs uses
  pure $ StoreExpression expr mempty mempty mempty types

-- to make a store expression we need to
-- a) work out all the deps this expression has
--   - values
--   - infix operators
--   - type constructors
--   - type names
-- b) map them to specific ExprHashes
exprToStoreExpression ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann)) ->
  (Expr Name (Type ann), Set Entity) ->
  m (StoreExpression (Type ann))
exprToStoreExpression compiledModules inputModule inputs (expr, uses) = do
  bindings <- bindingsFromEntities compiledModules inputModule inputs uses
  infixes <- infixesFromEntities inputs uses
  constructors <- constructorsFromEntities compiledModules inputModule inputs uses
  types <- typesFromEntities compiledModules inputModule inputs uses
  pure $ StoreExpression expr bindings constructors infixes types

-- given our dependencies and the entities used by the expression, create the
-- bindings
bindingsFromEntities ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann)) ->
  Set Entity ->
  m (Map (Maybe ModuleName, Name) ExprHash)
bindingsFromEntities compiledModules inputModule inputs uses = do
  let fromUse = \case
        EName name -> case M.lookup (DIName name) inputs of
          Just se -> pure $ M.singleton (Nothing, name) (getStoreExpressionHash se)
          _ -> throwError (ModuleErr $ CannotFindValues (S.singleton (DIName name)))
        ENamespacedName modName name ->
          case resolveNamespacedName compiledModules inputModule modName name of
            Just hash -> pure $ M.singleton (Just modName, name) hash
            _ -> pure mempty -- should this be an error?
        _ -> pure mempty

  -- combine results
  mconcat <$> traverse fromUse (S.toList uses)

-- given our dependencies and the entities used by the expression, create the
-- bindings
infixesFromEntities ::
  (MonadError (Error Annotation) m) =>
  Map DefIdentifier (StoreExpression (Type ann)) ->
  Set Entity ->
  m (Map InfixOp ExprHash)
infixesFromEntities inputs uses = do
  let fromUse = \case
        EInfix infixOp -> case M.lookup (DIInfix infixOp) inputs of
          Just se -> pure $ M.singleton infixOp (getStoreExpressionHash se)
          _ -> throwError (ModuleErr $ CannotFindValues (S.singleton (DIInfix infixOp)))
        _ -> pure mempty

  -- combine results
  mconcat <$> traverse fromUse (S.toList uses)

-- turns a bunch of StoreExpressions into a Store
toStore :: Map a (StoreExpression ann) -> Store ann
toStore = Store . M.fromList . fmap (\a -> (getStoreExpressionHash a, a)) . M.elems

-- | where can I find this function?
resolveNamespacedName ::
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  ModuleName ->
  Name ->
  Maybe ExprHash
resolveNamespacedName compiledModules inputModule modName name = do
  -- find out which module the modName refers to
  modHash <- M.lookup modName (moNamedImports inputModule)
  -- find the module in our pile of already compiled modules
  compiledMod <- M.lookup modHash compiledModules
  -- lookup the name in the module
  M.lookup (DIName name) (cmExprs compiledMod)

lookupImportedModule :: Module ann -> ModuleName -> Either ModuleError ModuleHash
lookupImportedModule inputModule modName =
  let namedImports = moNamedImports inputModule
   in case M.lookup modName namedImports of
        Just modHash -> pure modHash
        Nothing ->
          throwError
            ( NamedImportNotFound
                (M.keysSet namedImports)
                modName
            )

lookupCompiledModule ::
  Map ModuleHash (CompiledModule ann) ->
  ModuleHash ->
  Either ModuleError (CompiledModule ann)
lookupCompiledModule mods modHash = do
  case M.lookup modHash mods of
    Just foundModule -> pure foundModule
    _ -> throwError (MissingModule modHash)

lookupCompiledDef :: CompiledModule ann -> DefIdentifier -> Either ModuleError ExprHash
lookupCompiledDef compiledModule defId =
  let compiledExprs = cmExprs compiledModule
   in case M.lookup defId compiledExprs of
        Just exprHash -> pure exprHash
        Nothing -> throwError (CannotFindValues (S.singleton defId))

-- filter data types out, and put in a map keyed by TyCon
dataTypesByTyCon ::
  Map DefIdentifier (StoreExpression (Type ann)) ->
  Map
    TyCon
    ( StoreExpression (Type ann)
    )
dataTypesByTyCon items =
  let withSe se =
        fmap (se,)
          . listToMaybe
          . S.toList
          . extractDataTypes
          . storeExpression
          $ se

      dataTypes = mapMaybe withSe (M.elems items)
   in mconcat $
        ( \(se, DataType _ _ constructors) ->
            constructors $> se
        )
          <$> dataTypes

flattenCompiled ::
  CompiledModule (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann))
flattenCompiled cm =
  let lookupHash exprHash =
        M.lookup exprHash (getStore $ cmStore cm)
   in M.mapMaybe lookupHash (cmExprs cm)

-- | given our dependencies and the entities used by the expressions, create
-- the type bindings
constructorsFromEntities ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann)) ->
  Set Entity ->
  m (Map (Maybe ModuleName, TyCon) ExprHash)
constructorsFromEntities compiledModules inputModule inputs uses = do
  let fromUse = \case
        EConstructor tyCon ->
          case getStoreExpressionHash <$> M.lookup tyCon (dataTypesByTyCon inputs) of
            Just exprHash -> pure $ M.singleton (Nothing, tyCon) exprHash
            _ -> throwError (ModuleErr $ CannotFindConstructors (S.singleton tyCon))
        ENamespacedConstructor modName tyCon -> liftEither $
          first ModuleErr $ do
            -- find out which module the modName refers to
            modHash <- lookupImportedModule inputModule modName

            -- find the module in our pile of already compiled modules
            compiledMod <- lookupCompiledModule compiledModules modHash

            -- lookup the name in the module
            case M.lookup tyCon (dataTypesByTyCon (flattenCompiled compiledMod)) of
              Just se -> pure $ M.singleton (Just modName, tyCon) (getStoreExpressionHash se)
              Nothing -> throwError (CannotFindConstructors (S.singleton tyCon))
        _ -> pure mempty

  -- combine results
  mconcat <$> traverse fromUse (S.toList uses)

-- this is weird and probably bad, but if we can't find a type, just look
-- through all the other shit we found and look for something with the right
-- name
-- maybe in future to reduce how bad this is, explode if more than one are found?
findTypeInCompiled ::
  Map ModuleHash (CompiledModule (Type ann)) ->
  TypeName ->
  Either ModuleError ExprHash
findTypeInCompiled compiledModules typeName = do
  let lookInCompiled cm =
        M.lookup (DIType typeName) (cmExprs cm)
          >>= flip M.lookup (getStore $ cmStore cm)
  case listToMaybe $ mapMaybe lookInCompiled (M.elems compiledModules) of
    Just exprHash -> pure (getStoreExpressionHash exprHash)
    _ -> throwError (CannotFindTypes (S.singleton typeName))

-- | given our dependencies and the entities used by the expressions, create
-- the type bindings
typesFromEntities ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann)) ->
  Set Entity ->
  m (Map (Maybe ModuleName, TypeName) ExprHash)
typesFromEntities compiledModules inputModule inputs uses = do
  let fromUse = \case
        EType typeName ->
          case getStoreExpressionHash <$> M.lookup (DIType typeName) inputs of
            Just exprHash -> pure $ M.singleton (Nothing, typeName) exprHash
            _ -> throwError (ModuleErr $ CannotFindTypes (S.singleton typeName))
        ENamespacedType modName typeName ->
          liftEither $
            first ModuleErr $ do
              let perhapsExprHash = do
                    -- find out which module the modName refers to
                    modHash <- lookupImportedModule inputModule modName
                    -- find the module in our pile of already compiled modules
                    compiledMod <- lookupCompiledModule compiledModules modHash
                    -- lookup the name in the module
                    lookupCompiledDef compiledMod (DIType typeName)

              exprHash <- case perhapsExprHash of
                Right exprHash -> pure exprHash
                Left _ -> findTypeInCompiled compiledModules typeName
              -- return it
              pure (M.singleton (Just modName, typeName) exprHash)
        _ -> pure mempty

  -- combine results
  mconcat <$> traverse fromUse (S.toList uses)
