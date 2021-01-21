{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Evaluate
  ( doEvaluate,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Printer
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

doEvaluate ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doEvaluate env input expr = do
  (ResolvedExpression type' _ expr' scope' swaps) <-
    liftRepl $ getTypecheckedStoreExpression input env expr
  simplified <- liftRepl (first InterpreterErr (interpret scope' swaps expr'))
  replPrint $
    prettyPrint simplified
      <> "\n::\n"
      <> prettyPrint type'

---------
