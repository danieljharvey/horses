{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Actions.ResolveStoreExpression
  ( resolveStoreExpression,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
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

-- make the type map ok
makeTypeMap ::
  Map ExprHash (ResolvedExpression Annotation) ->
  StoreExpression Annotation ->
  Actions.ActionM (Map Name MonoType)
makeTypeMap resolvedDeps se = do
  let lookupRe exprHash = case M.lookup exprHash resolvedDeps of
        Just re -> pure (reMonoType re)
        Nothing -> throwError (StoreErr (CouldNotFindStoreExpression exprHash))
  traverse lookupRe (getBindings (storeBindings se))

-- | "better" version of this
substituteAndTypecheck ::
  Map ExprHash (ResolvedExpression Annotation) ->
  (StoreExpression Annotation, Text) ->
  Actions.ActionM (ResolvedExpression Annotation)
substituteAndTypecheck resolvedDeps (storeExpr, input) = do
  project <- Actions.getProject
  let (SubstitutedExpression swaps newExpr scope _deps typeDeps) =
        substitute (prjStore project) storeExpr
  typeMap <- makeTypeMap resolvedDeps storeExpr
  (_, _, typedExpr, exprType) <-
    getType typeMap typeDeps swaps input newExpr
  pure
    ( ResolvedExpression
        exprType
        storeExpr
        newExpr
        scope
        swaps
        typedExpr
        input
    )

-- given a store expression we want resolved we must
-- 1) recursively get all its deps as StoreExpressions
-- 2) turn them into a Build.State shape
-- 3) run the builder
-- 4) pick out the one we need
-- 5) (later) cache them to save time later
resolveStoreExpressions ::
  StoreExpression Annotation ->
  Text ->
  Actions.ActionM (Map ExprHash (ResolvedExpression Annotation))
resolveStoreExpressions se input = do
  let job resolvedDeps thisSe = do
        let input' =
              if getStoreExpressionHash thisSe == getStoreExpressionHash se
                then input
                else prettyPrint thisSe
        substituteAndTypecheck resolvedDeps (thisSe, input')
  -- get store expressions for all deps
  inputs <- getDepsForStoreExpression se
  -- add the main store expression
  let inputStoreExpressions = inputs <> M.singleton (getStoreExpressionHash se) se

  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  let state =
        Build.State
          { Build.stInputs =
              ( \storeExpr ->
                  Build.Plan
                    { Build.jbDeps =
                        S.fromList
                          ( M.elems (getBindings (storeBindings storeExpr))
                              <> M.elems (getTypeBindings (storeTypeBindings storeExpr))
                          ),
                      Build.jbInput = storeExpr
                    }
              )
                <$> inputStoreExpressions,
            Build.stOutputs = mempty -- here we could reuse cached items to save on building
          }
  Build.stOutputs <$> Build.doJobs job state

resolveStoreExpression ::
  StoreExpression Annotation ->
  Text ->
  Actions.ActionM (ResolvedExpression Annotation)
resolveStoreExpression se input = do
  resolved <- resolveStoreExpressions se input
  -- cache them here later maybe?
  case M.lookup (getStoreExpressionHash se) resolved of
    Just re -> pure re
    _ -> throwError (StoreErr (CouldNotFindStoreExpression (getStoreExpressionHash se)))

-- recursively get all the StoreExpressions required
getDepsForStoreExpression ::
  StoreExpression Annotation ->
  Actions.ActionM (Map ExprHash (StoreExpression Annotation))
getDepsForStoreExpression storeExpr = do
  project <- Actions.getProject
  depsList <- liftEither $ first StoreErr (recursiveResolve (prjStore project) storeExpr)
  pure
    ( M.fromList
        ( ( \se ->
              (getStoreExpressionHash se, se)
          )
            <$> depsList
        )
    )
