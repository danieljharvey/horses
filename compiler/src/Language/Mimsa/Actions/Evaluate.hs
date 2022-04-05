{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Evaluate
  ( evaluate,
  )
where

import Control.Monad.Except
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Prettyprinter

evaluate ::
  Text ->
  Expr Name Annotation ->
  Actions.ActionM
    ( MonoType,
      Expr Name Annotation,
      StoreExpression Annotation,
      Expr Name MonoType,
      Text
    )
evaluate input expr = do
  project <- Actions.getProject
  -- typecheck expression
  resolved <-
    Actions.typecheckExpression project input expr

  let se = reStoreExpression resolved

  -- get dependencies of StoreExpression
  depsSe <- Actions.getDepsForStoreExpression se

  -- optimise them all like a big legend
  storeExprs <- Actions.optimiseAll (fst <$> depsSe)

  -- get new root StoreExpression (it may be different due to optimisation)
  optimisedStoreExpr <- case M.lookup (getStoreExpressionHash se) storeExprs of
    Just re -> pure re
    _ -> throwError (StoreErr (CouldNotFindStoreExpression (getStoreExpressionHash se)))

  -- original expr with types and Name
  typedNameExpr <-
    Actions.useSwaps (reSwaps resolved) (reTypedExpression resolved)

  -- interpret
  interpretedExpr <-
    Actions.interpreter optimisedStoreExpr

  -- print any warnings
  traverse_ (Actions.appendMessage . prettyPrint) (getWarnings resolved)

  -- print
  Actions.appendDocMessage
    ( group
        ( prettyDoc interpretedExpr
            <> line
            <> "::"
            <> line
            <> prettyDoc (reMonoType resolved)
        )
    )
  pure (reMonoType resolved, interpretedExpr, optimisedStoreExpr, typedNameExpr, input)

---------
