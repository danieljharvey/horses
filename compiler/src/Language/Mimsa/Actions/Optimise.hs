{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Optimise
  ( optimise,
    optimiseByName,
    optimiseStoreExpression,
    optimiseAll,
  )
where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.FindExistingBinding as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Helpers.UpdateTests as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.AsNewtype
import Language.Mimsa.Transform.BetaReduce
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Transform.FlattenLets
import Language.Mimsa.Transform.FloatDown
import Language.Mimsa.Transform.FloatUp
import Language.Mimsa.Transform.Inliner
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Transform.SimplifyPatterns
import Language.Mimsa.Transform.TrimDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

optimiseByName :: Name -> Actions.ActionM (ResolvedExpression Annotation, Int)
optimiseByName name = do
  project <- Actions.getProject
  -- find existing expression matching name
  case Actions.findExistingBinding name project of
    -- there is an existing one, use its deps when evaluating
    Just se -> do
      -- make new se
      (resolved, numTestsUpdated) <- optimise se

      let newSe = reStoreExpression resolved

      -- bind it to `name`
      Actions.bindStoreExpression newSe name

      -- output message for repl
      Actions.appendDocMessage
        ( if se == newSe
            then "No changes in " <> prettyDoc name
            else
              "Optimised " <> prettyDoc name
                <> ". New expression: "
                <> prettyDoc (storeExpression newSe)
        )
      -- return it
      pure (resolved, numTestsUpdated)

    -- no existing binding, error
    Nothing ->
      throwError $ StoreErr $ CouldNotFindBinding name

-- | given an expression, optimise it and create a new StoreExpression
-- | this now accepts StoreExpression instead of expression
optimise ::
  StoreExpression Annotation ->
  Actions.ActionM (ResolvedExpression Annotation, Int)
optimise se = do
  project <- Actions.getProject

  -- run optimisations
  storeExprNew <- optimiseStoreExpression se

  -- typecheck optimisations
  resolvedNew <-
    Actions.checkStoreExpression
      (prettyPrint storeExprNew)
      project
      storeExprNew

  -- update tests
  numTestsUpdated <-
    Actions.updateTests
      (getStoreExpressionHash se)
      (getStoreExpressionHash storeExprNew)

  pure (resolvedNew, numTestsUpdated)

inlineExpression :: (Ord ann) => Expr Variable ann -> Expr Variable ann
inlineExpression =
  repeatUntilEq
    ( floatUp . flattenLets . simplifyPatterns . removeUnused
        . betaReduce
        . inline
    )

optimiseStoreExpression ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression Annotation)
optimiseStoreExpression storeExpr =
  do
    project <- Actions.getProject

    -- get Expr Variable ann
    resolvedOld <-
      Actions.checkStoreExpression
        (prettyPrint storeExpr)
        project
        storeExpr

    -- do the shit
    let optimised = inlineExpression (reVarExpression resolvedOld)

    -- make into Expr Name
    floatedUpExprName <- Actions.useSwaps (reSwaps resolvedOld) optimised

    -- float lets down into patterns
    let floatedSe =
          trimDeps
            (reStoreExpression resolvedOld)
            (floatDown floatedUpExprName)

    -- turn back into Expr Variable (fresh names for copied vars)
    resolvedFloated <-
      Actions.checkStoreExpression
        (prettyPrint (storeExpression floatedSe))
        project
        floatedSe

    -- remove unused stuff
    newExprName <-
      Actions.useSwaps
        (reSwaps resolvedFloated)
        (inlineExpression (reVarExpression resolvedFloated))

    let newStoreExpr =
          trimDeps
            (reStoreExpression resolvedFloated)
            newExprName

    -- save new store expr
    Actions.appendStoreExpression
      newStoreExpr

    pure newStoreExpr

updateBindings :: Map ExprHash ExprHash -> Bindings -> Bindings
updateBindings swaps (Bindings bindings) =
  Bindings $
    ( \exprHash -> case M.lookup exprHash swaps of
        Just newExprHash -> newExprHash
        _ -> exprHash
    )
      <$> bindings

updateTypeBindings :: Map ExprHash ExprHash -> TypeBindings -> TypeBindings
updateTypeBindings swaps (TypeBindings bindings) =
  TypeBindings $
    ( \exprHash -> case M.lookup exprHash swaps of
        Just newExprHash -> newExprHash
        _ -> exprHash
    )
      <$> bindings

--

-- given a StoreExpression and it's deps, return a Map of TyCon -> DataType
-- in the ActionM because it might fail (although probably won't)
createDataTypeMap ::
  Map ExprHash (StoreExpression ann) ->
  StoreExpression ann ->
  Actions.ActionM (Map TyCon DataType)
createDataTypeMap depMap se =
  let find exprHash = case M.lookup exprHash depMap of
        Just (StoreExpression (MyData _ dt _) _ _) -> pure dt
        _ -> throwError (TypeErr "" UnknownTypeError)
   in traverse find (getTypeBindings $ storeTypeBindings se)

-- Optimise a group of StoreExpressions
-- Currently optimises each one individually without using its parents
-- This should be a reasonably easy change to try in future though
optimiseAll ::
  Map ExprHash (StoreExpression Annotation) ->
  Actions.ActionM (Map ExprHash (StoreExpression Annotation))
optimiseAll inputStoreExpressions = do
  let action depMap se = do
        -- flatten newtypes
        dtMap <- createDataTypeMap depMap se
        let newtypedSe = se {storeExpression = asNewtype dtMap (storeExpression se)}

        -- optimise se
        optimisedSe <- optimiseStoreExpression newtypedSe
        let swaps = getStoreExpressionHash <$> depMap
        -- use the optimised deps passed in
        let newSe =
              optimisedSe
                { storeBindings = updateBindings swaps (storeBindings optimisedSe),
                  storeTypeBindings = updateTypeBindings swaps (storeTypeBindings optimisedSe)
                }
        -- store it
        Actions.appendStoreExpression newSe
        pure newSe

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
            Build.stOutputs = mempty -- we use caches here if we wanted
          }
  Build.stOutputs <$> Build.doJobs action state
