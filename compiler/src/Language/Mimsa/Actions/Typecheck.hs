{-# LANGUAGE FlexibleContexts #-}

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
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Typechecker
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.SubstitutedExpression
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

----------

getType ::
  Map Name MonoType ->
  Set (StoreExpression Annotation) ->
  Swaps ->
  Text ->
  Expr Variable Annotation ->
  Actions.ActionM
    ( Substitutions,
      [Constraint],
      Expr Variable MonoType,
      MonoType
    )
getType typeMap dataTypes swaps input expr = do
  liftEither $
    first
      (TypeErr input)
      ( typecheck
          typeMap
          swaps
          (createEnv typeMap dataTypes)
          expr
      )

-- given a big pile of resolved expressions
-- and some bindings, match them to the names and pull out their types
makeTypeMap ::
  Map ExprHash (ResolvedExpression Annotation) ->
  Bindings ->
  Actions.ActionM (Map Name MonoType)
makeTypeMap resolvedDeps bindings = do
  let lookupRe exprHash = case M.lookup exprHash resolvedDeps of
        Just re -> pure (reMonoType re)
        Nothing -> throwError (StoreErr (CouldNotFindStoreExpression exprHash))
  traverse lookupRe (getBindings bindings)

-- | "better" version of this
substituteAndTypecheck ::
  Map ExprHash (ResolvedExpression Annotation) ->
  (StoreExpression Annotation, Text) ->
  Actions.ActionM (ResolvedExpression Annotation)
substituteAndTypecheck resolvedDeps (storeExpr, input) = do
  project <- Actions.getProject
  let (SubstitutedExpression swaps newExpr _scope _deps typeDeps) =
        substitute (prjStore project) storeExpr
  typeMap <- makeTypeMap resolvedDeps (storeBindings storeExpr)
  (_, _, typedExpr, exprType) <-
    getType typeMap typeDeps swaps input newExpr
  pure
    ( ResolvedExpression
        exprType
        storeExpr
        newExpr
        swaps
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
                          ( M.elems (getBindings (storeBindings storeExpr))
                              <> M.elems (getTypeBindings (storeTypeBindings storeExpr))
                          ),
                      Build.jbInput = (storeExpr, input)
                    }
              )
                <$> inputStoreExpressions,
            Build.stOutputs = alreadyResolved
          }
  resolvedMap <- Build.stOutputs <$> Build.doJobs substituteAndTypecheck state
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
        inputStoreExpressions
          <> M.singleton (getStoreExpressionHash se) (se, input) -- overwrite root storeExpression so that we use the actual user input
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
  makeTypeMap resolvedMap bindings

annotateStoreExpressionWithTypes ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression MonoType)
annotateStoreExpressionWithTypes storeExpr = do
  project <- Actions.getProject

  -- make a new project that contains the StoreExpression's bindings
  let typecheckProject =
        Project
          (prjStore project)
          (bindingsToVersioned (storeBindings storeExpr))
          (typeBindingsToVersioned (storeTypeBindings storeExpr))
          mempty

  let exprName = storeExpression storeExpr

  -- re-typecheck the expression
  resolvedExpr <-
    typecheckExpression typecheckProject (prettyPrint exprName) exprName

  let typedExpr = reTypedExpression resolvedExpr

  -- swap Variables back for Names
  typedStoreExpr <-
    Actions.useSwaps (reSwaps resolvedExpr) typedExpr

  pure (storeExpr {storeExpression = typedStoreExpr})
