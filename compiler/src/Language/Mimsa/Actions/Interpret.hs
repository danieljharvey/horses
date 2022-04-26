{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Actions.Interpret (interpreter) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Interpreter.Interpret
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store

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
  allInterpreted <- interpretAll (fst <$> depsSe)

  case M.lookup (getStoreExpressionHash se) allInterpreted of
    Just re -> pure (bimap fst edAnnotation re)
    _ -> throwError (StoreErr (CouldNotFindStoreExpression (getStoreExpressionHash se)))

squashify :: (Ord e) => Map e (Map e a) -> Map e a
squashify = mconcat . M.elems

-- Interpret a group of StoreExpressions
-- This means each sub dep is only interpreted once
interpretAll ::
  Map ExprHash (StoreExpression Annotation) ->
  Actions.ActionM (Map ExprHash (InterpretExpr Name Annotation))
interpretAll inputStoreExpressions = do
  let action depMap se = do
        -- get us out of this Map of Maps situation
        let flatDeps = squashify depMap
        -- add numbers and mark imports
        numberedSe <- liftEither (first (TypeErr (prettyPrint se)) (addNumbers se))
        -- tag each `var` with it's location if it is an import
        let withImports = addEmptyStackFrames numberedSe
        -- interpret se
        interpreted <- liftEither (first InterpreterErr (interpret flatDeps withImports))
        -- we need to accumulate all deps
        -- as we go, so pass them up too
        let allDeps = flatDeps <> M.singleton (getStoreExpressionHash se) interpreted
        pure allDeps

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
  splat <- Build.stOutputs <$> Build.doJobs action state
  pure (squashify splat)
