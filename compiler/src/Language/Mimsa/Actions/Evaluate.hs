{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Evaluate
  ( evaluate,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Functor
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Interpreter.UseSwaps (useSwaps)
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
  resolved@(ResolvedExpression mt se expr' scope' swaps typedExpr input') <-
    liftEither $ Actions.getTypecheckedStoreExpression input project expr
  typedNameExpr <-
    liftEither
      ( first
          (\e -> InterpreterErr (e $> mempty))
          (useSwaps swaps typedExpr)
      )
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
