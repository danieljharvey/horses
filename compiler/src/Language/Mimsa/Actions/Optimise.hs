{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Optimise (optimise, optimiseByName) where

import Control.Monad.Except
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.FindExistingBinding as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Helpers.UpdateTests as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Transform.FlattenLets
import Language.Mimsa.Transform.FloatDown
import Language.Mimsa.Transform.FloatUp
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

  resolvedOld <-
    Actions.checkStoreExpression
      ( prettyPrint (storeExpression se)
      )
      project
      se

  -- flatten lets
  let flattened = flattenLets (reVarExpression resolvedOld)

  -- float lets up above lambdas
  let floatedUp = floatUp flattened

  -- make into Expr Name
  floatedUpExprName <- Actions.useSwaps (reSwaps resolvedOld) floatedUp

  -- float lets down into patterns
  let floatedDown = floatDown floatedUpExprName

  let floatedSe = trimDeps se floatedDown

  -- turn back into Expr Variable (fresh names for copied vars)
  resolvedFloated <-
    Actions.checkStoreExpression
      (prettyPrint (storeExpression floatedSe))
      project
      floatedSe

  let newExpr = removeUnused (reVarExpression resolvedFloated)

  newExprName <- Actions.useSwaps (reSwaps resolvedFloated) newExpr

  let newStoreExpr = trimDeps se newExprName

  resolved <- Actions.checkStoreExpression (prettyPrint (storeExpression newStoreExpr)) project newStoreExpr

  Actions.appendStoreExpression (reStoreExpression resolved)

  -- update tests
  numTestsUpdated <-
    Actions.updateTests
      (getStoreExpressionHash se)
      (getStoreExpressionHash newStoreExpr)

  pure (resolved, numTestsUpdated)
