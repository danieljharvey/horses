{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Modules.ToStoreExprs (toStoreExpressions, CompiledModule (..)) where

import Control.Monad.Except
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import Language.Mimsa.Modules.Dependencies
import Language.Mimsa.Modules.HashModule
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

toStoreExpression ::
  (MonadError (Error Annotation) m, Monoid ann) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann)) ->
  (DefIdentifier, DepType (Type ann), Set Entity) ->
  m (StoreExpression (Type ann))
toStoreExpression compiledModules inputModule inputs (_, dep, uses) =
  case dep of
    (DTExpr expr) -> exprToStoreExpression compiledModules inputModule inputs (expr, uses)
    (DTData dt) -> dataTypeToStoreExpression dt

-- this is crap, need to add type bindings
dataTypeToStoreExpression ::
  (Monad m, Monoid ann) =>
  DataType ->
  m (StoreExpression (Type ann))
dataTypeToStoreExpression dt =
  let mt = MTRecord mempty mempty -- it's weird MyData needs this, so give it rubbish
      expr = MyData mt dt (MyRecord mt mempty)
   in pure (StoreExpression expr mempty mempty mempty)

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
  typeBindings <- typesFromEntities compiledModules inputModule inputs uses
  pure $ StoreExpression expr bindings typeBindings infixes

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

--- compile a module into StoreExpressions
compileModuleDefinitions ::
  (MonadError (Error Annotation) m, Eq ann, Monoid ann) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  m (CompiledModule (Type ann))
compileModuleDefinitions compiledModules inputModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <-
    getValueDependencies inputModule

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

--- compile many modules
compileAllModules ::
  (MonadError (Error Annotation) m, Eq ann, Monoid ann, Show ann) =>
  Map ModuleHash (Module (Type ann)) ->
  Module (Type ann) ->
  m (Map ModuleHash (CompiledModule (Type ann)))
compileAllModules myDeps rootModule = do
  -- which other modules do we need to compile in order to compile this one?
  inputWithDeps <- getModuleDeps myDeps rootModule

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

-- | where can I find this Constructor?
resolveNamespacedTyCon ::
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  ModuleName ->
  TyCon ->
  Maybe ExprHash
resolveNamespacedTyCon compiledModules inputModule modName tyCon = do
  -- find out which module the modName refers to
  modHash <- M.lookup modName (moNamedImports inputModule)

  -- find the module in our pile of already compiled modules
  compiledMod <- M.lookup modHash compiledModules

  -- lookup the name in the module
  getStoreExpressionHash <$> M.lookup tyCon (dataTypesByTyCon (flattenCompiled compiledMod))

-- filter data types out, and put in a map keyed by TyCon
dataTypesByTyCon ::
  Map DefIdentifier (StoreExpression (Type ann)) ->
  Map
    TyCon
    ( StoreExpression (Type ann)
    )
dataTypesByTyCon items =
  let withSe se =
        fmap (se,) . listToMaybe . S.toList . extractDataTypes
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
typesFromEntities ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (CompiledModule (Type ann)) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression (Type ann)) ->
  Set Entity ->
  m (Map (Maybe ModuleName, TyCon) ExprHash)
typesFromEntities compiledModules inputModule inputs uses = do
  let fromUse = \case
        EConstructor tyCon ->
          case getStoreExpressionHash <$> M.lookup tyCon (dataTypesByTyCon inputs) of
            Just exprHash -> pure $ M.singleton (Nothing, tyCon) exprHash
            _ -> throwError (ModuleErr $ CannotFindConstructors (S.singleton tyCon))
        ENamespacedConstructor modName tyCon ->
          case resolveNamespacedTyCon compiledModules inputModule modName tyCon of
            Just hash -> pure $ M.singleton (Just modName, tyCon) hash
            _ -> error $ "could not resolve namespaced type " <> show modName <> "." <> show tyCon
        _ -> pure mempty

  -- combine results
  mconcat <$> traverse fromUse (S.toList uses)
