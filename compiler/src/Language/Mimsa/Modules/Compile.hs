{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Compile (compile, CompiledModule (..)) where

-- `compile` here means "turn it into a bunch of StoreExpressions"

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import Language.Mimsa.Modules.Dependencies
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

data CompiledModule ann = CompiledModule
  { cmStore :: Store ann,
    cmExprs :: Map DefIdentifier ExprHash
  }
  deriving stock (Eq, Ord, Show)

-- to make a store expression we need to
-- a) work out all the deps this expression has
--   - values
--   - infix operators
--   - type constructors
--   - type names
-- b) map them to specific ExprHashes
toStoreExpression ::
  Map DefIdentifier (StoreExpression ann) ->
  (DefIdentifier, Expr Name ann, Set Entity) ->
  CheckM (StoreExpression ann)
toStoreExpression inputs (_, expr, uses) =
  pure $ StoreExpression expr (bindingsFromEntities inputs uses) mempty

-- given our dependencies and the entities used by the expression, create the
-- bindings
bindingsFromEntities ::
  Map DefIdentifier (StoreExpression ann) ->
  Set Entity ->
  Bindings
bindingsFromEntities inputs uses =
  foldMap
    ( \case
        EName name -> case M.lookup (DIName name) inputs of
          Just se -> Bindings $ M.singleton name (getStoreExpressionHash se)
          _ -> error $ "Could not find binding for " <> T.unpack (prettyPrint name)
        _ -> mempty
    )
    (S.toList uses)

-- turns a bunch of StoreExpressions into a Store
toStore :: Map a (StoreExpression ann) -> Store ann
toStore = Store . M.fromList . fmap (\a -> (getStoreExpressionHash a, a)) . M.elems

compile :: (Eq ann, Monoid ann) => Module (Type ann) -> CheckM (CompiledModule ann)
compile inputModule = do
  storeExprs <- compileModuleDefinitions mempty inputModule
  pure $
    CompiledModule
      { cmStore = toStore storeExprs,
        cmExprs = getStoreExpressionHash <$> storeExprs
      }

--- compile a module into StoreExpressions
compileModuleDefinitions ::
  (Eq ann, Monoid ann) =>
  Map ModuleHash (Module (Type ann)) -> -- this input is surely wrong
  Module (Type ann) ->
  CheckM (Map DefIdentifier (StoreExpression ann))
compileModuleDefinitions _typecheckedDeps inputModule = do
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
  Build.stOutputs
    <$> Build.doJobs toStoreExpression state
