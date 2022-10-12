module Language.Mimsa.Actions.Optimise
  ( optimise,
    optimiseAll,
  )
where

-- this module is currently unused, we should be using it
-- to optimise StoreExpressions before Evaluating or Compiling them

import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.BetaReduce
import Language.Mimsa.Transform.EtaReduce
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Transform.FlattenLets
import Language.Mimsa.Transform.FloatDown
import Language.Mimsa.Transform.FloatUp
import Language.Mimsa.Transform.Inliner
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Transform.SimplifyPatterns
import Language.Mimsa.Transform.TrimDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- | given an expression, optimise it and create a new StoreExpression
-- | this now accepts StoreExpression instead of expression
optimise ::
  StoreExpression Annotation ->
  Actions.ActionM (ResolvedExpression Annotation)
optimise se = do
  project <- Actions.getProject

  -- run optimisations
  storeExprNew <- optimiseStoreExpression se

  -- typecheck optimisations
  Actions.checkStoreExpression
    (prettyPrint storeExprNew)
    project
    storeExprNew

inlineExpression :: (Ord ann, Ord var) => Expr var ann -> Expr var ann
inlineExpression =
  repeatUntilEq
    ( floatUp
        . flattenLets
        . simplifyPatterns
        . etaReduce
        . removeUnused
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
    -- we should do NumberVars directly to avoid needing
    -- to typecheck StoreExpressions anymore
    resolvedOld <-
      Actions.checkStoreExpression
        (prettyPrint storeExpr)
        project
        storeExpr

    -- do the shit
    let optimised = inlineExpression (reVarExpression resolvedOld)

    -- make into Expr Name
    let floatedUpExprName = first fst optimised

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
    let newExprName = first fst (inlineExpression (reVarExpression resolvedFloated))

    let newStoreExpr =
          trimDeps
            (reStoreExpression resolvedFloated)
            newExprName

    -- save new store expr
    Actions.appendStoreExpression
      newStoreExpr

    pure newStoreExpr

updateBindings :: Map ExprHash ExprHash -> Map (Maybe ModuleName, Name) ExprHash -> Map (Maybe ModuleName, Name) ExprHash
updateBindings swaps =
  fmap
    ( \exprHash -> case M.lookup exprHash swaps of
        Just newExprHash -> newExprHash
        _ -> exprHash
    )

updateTypeBindings :: Map ExprHash ExprHash -> Map k ExprHash -> Map k ExprHash
updateTypeBindings swaps bindings =
  ( \exprHash -> case M.lookup exprHash swaps of
      Just newExprHash -> newExprHash
      _ -> exprHash
  )
    <$> bindings

--

-- Optimise a group of StoreExpressions
-- Currently optimises each one individually without using its parents
-- This should be a reasonably easy change to try in future though
optimiseAll ::
  Map ExprHash (StoreExpression Annotation) ->
  Actions.ActionM (Map ExprHash (StoreExpression Annotation))
optimiseAll inputStoreExpressions = do
  let action depMap se = do
        -- optimise se
        optimisedSe <- optimiseStoreExpression se
        let swaps = getStoreExpressionHash <$> depMap
        -- use the optimised deps passed in
        let newSe =
              case optimisedSe of
                ose@StoreExpression {} ->
                  ose
                    { seBindings = updateBindings swaps (storeBindings optimisedSe),
                      seTypeBindings = updateTypeBindings swaps (storeTypeBindings optimisedSe)
                    }
                sd -> sd
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
                          ( M.elems (storeBindings storeExpr)
                              <> M.elems (storeTypeBindings storeExpr)
                          ),
                      Build.jbInput = storeExpr
                    }
              )
                <$> inputStoreExpressions,
            Build.stOutputs = mempty -- we use caches here if we wanted
          }
  Build.stOutputs <$> Build.doJobs action state
