{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Evaluate
  ( evaluate,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Printer
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

  -- optimise
  optimisedStoreExpr <- Actions.optimiseStoreExpression (reStoreExpression resolved)

  -- resolve optimised expression
  (ResolvedExpression mt se expr' scope' swaps typedExpr input') <-
    Actions.checkStoreExpression
      (prettyPrint optimisedStoreExpr)
      project
      optimisedStoreExpr

  -- expr with types and Name
  typedNameExpr <-
    Actions.useSwaps swaps typedExpr

  -- interpret
  interpretedExpr <-
    liftEither (first InterpreterErr (interpret scope' swaps expr'))

  -- print any warnings
  traverse_ (Actions.appendMessage . prettyPrint) (getWarnings resolved)

  -- print
  Actions.appendDocMessage
    ( group
        ( prettyDoc interpretedExpr
            <> line
            <> "::"
            <> line
            <> prettyDoc mt
        )
    )
  pure (mt, interpretedExpr, se, typedNameExpr, input')

---------
