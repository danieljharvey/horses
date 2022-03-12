{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Optimise
  ( optimise,
    optimiseByName,
    optimiseStoreExpression,
    optimiseWithDeps,
    useOptimisedStoreExpressionDeps,
  )
where

import Control.Monad.Except
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.FindExistingBinding as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Helpers.UpdateTests as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Transform.BetaReduce
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Transform.FlattenLets
import Language.Mimsa.Transform.FloatDown
import Language.Mimsa.Transform.FloatUp
import Language.Mimsa.Transform.InlineDeps
import Language.Mimsa.Transform.Inliner
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Transform.TrimDeps
import Language.Mimsa.Transform.UseOptimisedDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
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
    ( floatUp . flattenLets . removeUnused
        . betaReduce
        . inline
    )

lookupOptimisation ::
  Project Annotation ->
  StoreExpression Annotation ->
  Maybe (StoreExpression Annotation)
lookupOptimisation prj =
  lookupExprHash prj <=< lookupOptimised prj . getStoreExpressionHash

-- | update a StoreExpression to use the latest optimisations of its deps
-- this does not happen in the `optimiseStoreExpression` flow as it has
-- different results depending on the project state
-- so we only want to introduce this complexity explicitly
useOptimisedStoreExpressionDeps ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression Annotation)
useOptimisedStoreExpressionDeps se = do
  -- get latest project
  project <- Actions.getProject
  -- swap in StoreExpression deps for newest versions
  let seWithNewDeps = useOptimisedDeps project se
  -- store new StoreExpression and optimisation
  Actions.appendOptimisedStoreExpression (getStoreExpressionHash se) seWithNewDeps
  -- return it
  pure seWithNewDeps

-- | when we might be inlining our dependencies, we might need their deps too
-- lets just grab all the deps ever and discard the unused ones later
withAllDeps ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression Annotation)
withAllDeps se = do
  let allStoreExprHashes = M.elems $ getBindings (storeBindings se)
  storeExprs <- traverse Actions.lookupExpression allStoreExprHashes
  let allBindings = foldMap storeBindings storeExprs
  pure $ se {storeBindings = storeBindings se <> allBindings}

-- | optimise a StoreExpression, with potential to consider it's deps for
-- direct inlining
optimiseWithDeps ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression Annotation)
optimiseWithDeps se = do
  project <- Actions.getProject

  -- turn back into Expr Variable (fresh names for copied vars)
  resolvedExpr <-
    Actions.checkStoreExpression
      (prettyPrint se)
      project
      se

  -- remove unused stuff
  newExprName <-
    Actions.useSwaps
      (reSwaps resolvedExpr)
      (inlineExpression (inlineStoreExpression resolvedExpr))

  seWithManyDeps <-
    withAllDeps
      (reStoreExpression resolvedExpr)

  pure $
    trimDeps
      seWithManyDeps
      newExprName

optimiseStoreExpression ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression Annotation)
optimiseStoreExpression storeExpr = do
  project <- Actions.getProject

  case lookupOptimisation project storeExpr of
    Just optSe -> do
      -- output message for repl
      if optSe /= storeExpr
        then
          Actions.appendDocMessage
            ( "Using precalculated optimised expression for "
                <> prettyDoc (getStoreExpressionHash storeExpr)
            )
        else pure ()

      pure optSe
    Nothing -> do
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
      Actions.appendOptimisedStoreExpression
        (getStoreExpressionHash storeExpr)
        newStoreExpr

      pure newStoreExpr
