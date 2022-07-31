{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Actions.Typecheck
  ( typecheckStoreExpression,
    typecheckExpression,
    typeMapForProjectSearch,
    annotateStoreExpressionWithTypes,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Store.ResolveDataTypes
import Language.Mimsa.Typechecker
import Language.Mimsa.Typechecker.CreateEnv
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Unique

----------

getType ::
  Map Name MonoType ->
  Map (Maybe ModuleName, TypeName) DataType ->
  Text ->
  Expr (Name, Unique) Annotation ->
  Actions.ActionM
    ( Substitutions,
      [Constraint],
      Expr (Name, Unique) MonoType,
      MonoType
    )
getType depTypeMap dataTypes input expr = do
  liftEither $
    first
      (TypeErr input)
      ( typecheck
          depTypeMap
          (createEnv depTypeMap dataTypes mempty mempty)
          expr
      )

-- given a big pile of resolved expressions
-- and some bindings, match them to the names and pull out their types
makeTypeMap ::
  Map ExprHash (ResolvedExpression Annotation) ->
  Map (Maybe ModuleName, Name) ExprHash ->
  Actions.ActionM (Map Name MonoType)
makeTypeMap resolvedDeps bindings = do
  let lookupRe exprHash = case M.lookup exprHash resolvedDeps of
        Just re -> pure (reMonoType re)
        Nothing -> throwError (StoreErr (CouldNotFindStoreExpression exprHash))
  removeModuleNames <$> traverse lookupRe bindings

removeModuleNames :: (Ord k2) => Map (k1, k2) a -> Map k2 a
removeModuleNames = M.fromList . fmap (first snd) . M.toList

-- | "better" version of this
substituteAndTypecheck ::
  Map ExprHash (ResolvedExpression Annotation) ->
  (StoreExpression Annotation, Text) ->
  Actions.ActionM (ResolvedExpression Annotation)
substituteAndTypecheck resolvedDeps (storeExpr, input) = do
  project <- Actions.getProject
  numberedExpr <- liftEither $ first (TypeErr input) (addNumbersToStoreExpression storeExpr)
  depTypeMap <- makeTypeMap resolvedDeps (storeBindings storeExpr)
  dataTypes <- liftEither $ first StoreErr (resolveDataTypes (prjStore project) storeExpr)
  (_, _, typedExpr, exprType) <-
    getType depTypeMap dataTypes input numberedExpr
  pure
    ( ResolvedExpression
        exprType
        storeExpr
        numberedExpr
        typedExpr
        input
    )

-- given a store expression we want typechecked we must
-- 1) recursively get all its deps as StoreExpressions
-- 2) turn them into a Build.State shape
-- 3) run the builder
-- 4) pick out the one we need
-- 5) cache them to save time later
typecheckStoreExpressions ::
  Map ExprHash (StoreExpression Annotation, Text) ->
  Actions.ActionM (Map ExprHash (ResolvedExpression Annotation))
typecheckStoreExpressions inputStoreExpressions = do
  alreadyResolved <- Actions.getResolvedExpressions
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  let state =
        Build.State
          { Build.stInputs =
              ( \(storeExpr, input) ->
                  Build.Plan
                    { Build.jbDeps =
                        S.fromList
                          ( M.elems (storeBindings storeExpr)
                              <> M.elems (storeTypeBindings storeExpr)
                          ),
                      Build.jbInput = (storeExpr, input)
                    }
              )
                <$> inputStoreExpressions,
            Build.stOutputs = alreadyResolved
          }
  resolvedMap <-
    Build.stOutputs
      <$> Build.doJobs substituteAndTypecheck state
  -- cache all the resolved expressions
  traverse_
    (uncurry Actions.appendResolvedExpression)
    (M.toList resolvedMap)
  pure resolvedMap

typecheckStoreExpression ::
  StoreExpression Annotation ->
  Text ->
  Actions.ActionM (ResolvedExpression Annotation)
typecheckStoreExpression se input = do
  inputStoreExpressions <- Actions.getDepsForStoreExpression se
  let allInputs =
        M.singleton (getStoreExpressionHash se) (se, input) -- overwrite root storeExpression so that we use the actual user input
          <> inputStoreExpressions
  resolved <- typecheckStoreExpressions allInputs
  -- cache all the resolved expressions
  traverse_
    (uncurry Actions.appendResolvedExpression)
    (M.toList resolved)
  case M.lookup (getStoreExpressionHash se) resolved of
    Just re -> pure re
    _ -> throwError (StoreErr (CouldNotFindStoreExpression (getStoreExpressionHash se)))

-- | get an expression, capture deps from project, and typecheck it
typecheckExpression ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  Actions.ActionM (ResolvedExpression Annotation)
typecheckExpression project input expr = do
  storeExpr <-
    liftEither $
      first ResolverErr $
        createStoreExpression
          (getCurrentBindings $ prjBindings project)
          (getCurrentTypeBindings $ prjTypeBindings project)
          expr
  typecheckStoreExpression storeExpr input

-- | get types for every top-level expression bound in the project
-- | 1. get all top-level exprhashes
-- | 2. turn them into store exprs
-- | 3. get their dependency StoreExpressions too
-- | 4. typecheck them
-- | 5. make them into a type map
typeMapForProjectSearch :: Actions.ActionM (Map Name MonoType)
typeMapForProjectSearch = do
  project <- Actions.getProject
  -- get all top level items
  let bindings = getCurrentBindings . prjBindings $ project
  -- fetch StoreExpressions for top-level bindings
  storeExprs <- traverse Actions.lookupExpression (M.elems (getBindings bindings))
  -- also fetch deps of said bindings
  manyMaps <- traverse Actions.getDepsForStoreExpression storeExprs
  -- typecheck everything
  resolvedMap <- typecheckStoreExpressions (mconcat manyMaps)
  -- make into a nice type map
  makeTypeMap resolvedMap (bindingsToModuleThing bindings)

bindingsToModuleThing :: Bindings -> Map (Maybe ModuleName, Name) ExprHash
bindingsToModuleThing (Bindings b) =
  M.fromList
    . fmap (first (Nothing,))
    . M.toList
    $ b

-- | re-typecheck a single store expression
-- not sure how this is different from other typechecking fns now
annotateStoreExpressionWithTypes ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression MonoType)
annotateStoreExpressionWithTypes storeExpr = do
  -- re-typecheck the expression
  resolvedExpr <-
    typecheckStoreExpression storeExpr (prettyPrint storeExpr)

  -- swap (Name, Unique) back for Names
  let typedStoreExpr = first fst (reTypedExpression resolvedExpr)

  pure (storeExpr {storeExpression = typedStoreExpr})
