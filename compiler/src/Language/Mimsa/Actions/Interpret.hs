{-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Mimsa.Actions.Interpret (interpreter) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.NumberStoreExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Interpret
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Store
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store

-- cheeky orphaned instance
instance (Eq ann, Ord x) => Eq (HOAS.HOASExpr (Name,x) ann) where
  a == b = fromHOAS a == fromHOAS b

-- get all the deps
-- change var to InterpretVar to point at imports
-- also collect Map ExprHash (Expr InterpretVar ann)
-- then interpret it
-- ...
-- profit?
interpreter :: StoreExpression Annotation -> Actions.ActionM (Expr Name Annotation)
interpreter se = do
  -- get dependencies of StoreExpression
  depsSe <- Actions.getDepsForStoreExpression se

  -- optimise them all like a big legend
  allOptimised <- Actions.optimiseAll (fst <$> depsSe)

  -- what is this rootExprHash now we've messed with everything
  newRootExprHash <- case M.lookup (getStoreExpressionHash se) allOptimised of
    Just re -> pure (getStoreExpressionHash re)
    _ -> throwError (StoreErr (CouldNotFindStoreExpression (getStoreExpressionHash se)))

  -- interpret everything
  allInterpreted <- interpretAll (fixKeys allOptimised)

  -- pick out the value we're interested in
  case M.lookup newRootExprHash allInterpreted of
    Just re -> pure (first fst (fromHOAS re))
    _ -> throwError (StoreErr (CouldNotFindStoreExpression newRootExprHash))

fixKeys :: Map ExprHash (StoreExpression Annotation) -> Map ExprHash (StoreExpression Annotation)
fixKeys = foldMap (\se -> M.singleton (getStoreExpressionHash se) se) . M.elems

squashify :: (Ord e) => Map e (Map e a) -> Map e a
squashify = mconcat . M.elems

-- Interpret a group of StoreExpressions
-- This means each sub dep is only interpreted once
interpretAll ::
  Map ExprHash (StoreExpression Annotation) ->
  Actions.ActionM (Map ExprHash (InterpretExpr Annotation))
interpretAll inputStoreExpressions = do
  let action depMap se =
        case storeExpression se of
          Just expr -> do
            -- get us out of this Map of Maps situation
            let flatDeps = squashify depMap

            -- add numbers and mark imports
            numberedSe <-
              Actions.numberStoreExpression expr (storeBindings se)

            -- get exprhashes for any infixOps we need
            let infixHashes = storeInfixes se
            -- interpret se
            interpreted <-
              liftEither
                ( first
                    InterpreterErr
                    (interpret flatDeps infixHashes (toHOAS numberedSe))
                )

            -- we need to accumulate all deps
            -- as we go, so pass them up too
            let allDeps = flatDeps <> M.singleton (getStoreExpressionHash se) interpreted
            pure allDeps
          Nothing -> pure mempty -- do nothing with DataType

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
                              <> M.elems (storeInfixes storeExpr)
                          ),
                      Build.jbInput = storeExpr
                    }
              )
                <$> inputStoreExpressions,
            Build.stOutputs = mempty -- we use caches here if we wanted
          }
  -- go!
  squashify . Build.stOutputs <$> Build.doJobs action state
