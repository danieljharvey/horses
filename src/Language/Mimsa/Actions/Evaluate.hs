{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Evaluate
  ( evaluate,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Text (Text)
import Language.Mimsa.Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Typechecker

evaluate ::
  Text ->
  Expr Name Annotation ->
  Actions.ActionM (MonoType, Expr Variable Annotation)
evaluate input expr = do
  project <- Actions.getProject
  (ResolvedExpression mt _ expr' scope' swaps) <-
    liftEither $ getTypecheckedStoreExpression input project expr
  interpretedExpr <-
    liftEither (first InterpreterErr (interpret scope' swaps expr'))
  Actions.appendMessage
    ( prettyPrint interpretedExpr
        <> "\n::\n"
        <> prettyPrint mt
    )
  pure (mt, interpretedExpr)

---------
