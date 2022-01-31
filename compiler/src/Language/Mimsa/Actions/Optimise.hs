{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Optimise (optimise, optimiseByName) where

import Control.Monad.Except
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.FindExistingBinding as Actions
import qualified Language.Mimsa.Actions.Helpers.UpdateTests as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.FindUnused
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
  let unused = findUnused (storeExpression se)
      newExpr = removeUnused (S.map fst unused) (storeExpression se)
  let newStoreExpr = trimDeps se newExpr
  project <- Actions.getProject

  resolved <- Actions.checkStoreExpression (prettyPrint (storeExpression newStoreExpr)) project newStoreExpr

  Actions.appendStoreExpression (reStoreExpression resolved)

  -- update tests
  numTestsUpdated <-
    Actions.updateTests
      (getStoreExpressionHash se)
      (getStoreExpressionHash newStoreExpr)

  pure (resolved, numTestsUpdated)
