module Language.Mimsa.Actions.Optimise
  ( optimiseAll,
  )
where

-- this module is currently unused, we should be using it
-- to optimise StoreExpressions before Evaluating or Compiling them

import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import qualified Language.Mimsa.Actions.Helpers.NumberStoreExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Core
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
import Language.Mimsa.Types.Store

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
    case storeExpression storeExpr of
      Nothing -> pure storeExpr
      Just expr -> do
        let originalBindings = storeBindings storeExpr

        withNumbers <-
          Actions.numberStoreExpression expr originalBindings

        -- do the shit
        let optimised = inlineExpression withNumbers

        -- make into Expr Name
        let floatedUpExprName = first fst optimised

        -- float lets down into patterns
        let floatedSe =
              floatDown floatedUpExprName

        -- turn back into Expr Variable (fresh names for copied vars)
        floatedWithNumbers <-
          Actions.numberStoreExpression floatedSe originalBindings

        -- remove unused stuff
        let newStoreExpr = trimDeps storeExpr (first fst (inlineExpression floatedWithNumbers))

        -- save new store expr
        Actions.appendStoreExpression
          newStoreExpr

        pure newStoreExpr

useSwaps :: Map ExprHash ExprHash -> Map k ExprHash -> Map k ExprHash
useSwaps swaps bindings =
  ( \exprHash -> case M.lookup exprHash swaps of
      Just newExprHash -> newExprHash
      _ -> exprHash
  )
    <$> bindings

--

-- Optimise a group of StoreExpressions
optimiseAll ::
  Map ExprHash (StoreExpression Annotation) ->
  Actions.ActionM (Map ExprHash (StoreExpression Annotation))
optimiseAll inputStoreExpressions = do
  let action depMap se = do
        -- optimise se
        optimisedSe <- optimiseStoreExpression se
        -- make a map of expected hash with new actual hash for swapping
        let swaps = getStoreExpressionHash <$> depMap
        -- use the optimised deps passed in
        let newSe =
              case optimisedSe of
                ose@StoreExpression {} ->
                  ose
                    { seBindings = useSwaps swaps (storeBindings optimisedSe),
                      seTypeBindings = useSwaps swaps (storeTypeBindings optimisedSe),
                      seInfixes = useSwaps swaps (storeInfixes optimisedSe),
                      seTypes = useSwaps swaps (storeTypes optimisedSe)
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
