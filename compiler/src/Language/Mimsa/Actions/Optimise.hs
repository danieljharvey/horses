{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Optimise (optimise, optimiseByName, optimiseStoreExpression) where

import Control.Monad.Except
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.FindExistingBinding as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Helpers.UpdateTests as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.BetaReduce
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Transform.FlattenLets
import Language.Mimsa.Transform.FloatDown
import Language.Mimsa.Transform.FloatUp
import Language.Mimsa.Transform.Inliner
import Language.Mimsa.Transform.Shared
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

  -- save new store expr
  Actions.appendStoreExpression storeExprNew

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
    ( floatUp . flattenLets . removeUnused
        . betaReduce
        . inline
    )

optimiseStoreExpression ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression Annotation)
optimiseStoreExpression storeExpr = do
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

  pure $
    trimDeps
      (reStoreExpression resolvedFloated)
      newExprName
