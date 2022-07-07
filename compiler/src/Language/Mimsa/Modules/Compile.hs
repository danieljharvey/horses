{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Compile (compile, CompiledModule (..)) where

-- `compile` here means "turn it into a bunch of StoreExpressions"

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import Language.Mimsa.Modules.Dependencies
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Store
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
  deriving stock (Eq, Ord, Show)

instance Semigroup (CompiledModule ann) where
  (CompiledModule a b) <> (CompiledModule a' b') =
    CompiledModule (a <> a') (b <> b')

instance Monoid (CompiledModule ann) where
  mempty = CompiledModule mempty mempty

toStoreExpression ::
  (Monoid ann) =>
  Map ModuleHash (CompiledModule ann) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression ann) ->
  (DefIdentifier, DepType ann, Set Entity) ->
  CheckM (StoreExpression ann)
toStoreExpression compiledModules inputModule inputs (_, dep, uses) =
  case dep of
    (DTExpr expr) -> exprToStoreExpression compiledModules inputModule inputs (expr, uses)
    (DTData dt) -> dataTypeToStoreExpression dt

-- this is crap, need to add type bindings
dataTypeToStoreExpression ::
  (Monoid ann) =>
  DataType ->
  CheckM (StoreExpression ann)
dataTypeToStoreExpression dt =
  let expr = MyData mempty dt (MyRecord mempty mempty)
   in pure (StoreExpression expr mempty mempty mempty)

-- to make a store expression we need to
-- a) work out all the deps this expression has
--   - values
--   - infix operators
--   - type constructors
--   - type names
-- b) map them to specific ExprHashes
exprToStoreExpression ::
  Map ModuleHash (CompiledModule ann) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression ann) ->
  (Expr Name ann, Set Entity) ->
  CheckM (StoreExpression ann)
exprToStoreExpression compiledModules inputModule inputs (expr, uses) = do
  bindings <- bindingsFromEntities compiledModules inputModule inputs uses
  infixes <- infixesFromEntities inputs uses
  pure $ StoreExpression expr bindings mempty infixes

resolveNamespacedName ::
  Map ModuleHash (CompiledModule ann) ->
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

-- given our dependencies and the entities used by the expression, create the
-- bindings
bindingsFromEntities ::
  Map ModuleHash (CompiledModule ann) ->
  Module (Type ann) ->
  Map DefIdentifier (StoreExpression ann) ->
  Set Entity ->
  CheckM (Map (Maybe ModuleName, Name) ExprHash)
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
  Map DefIdentifier (StoreExpression ann) ->
  Set Entity ->
  CheckM (Map InfixOp ExprHash)
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

compile ::
  (Eq ann, Monoid ann, Show ann) =>
  Map ModuleHash (Module (Type ann)) ->
  Module (Type ann) ->
  CheckM (CompiledModule ann)
compile typecheckedModules inputModule = do
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
  (Eq ann, Monoid ann) =>
  Map ModuleHash (CompiledModule ann) ->
  Module (Type ann) ->
  CheckM (CompiledModule ann)
compileModuleDefinitions compiledModules inputModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <-
    getValueDependencies
      (getAnnotationForType <$> inputModule) -- we're going to need the types soon to extract which types are returned etc
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
  (Eq ann, Monoid ann, Show ann) =>
  Map ModuleHash (Module (Type ann)) ->
  Module (Type ann) ->
  CheckM (Map ModuleHash (CompiledModule ann))
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
