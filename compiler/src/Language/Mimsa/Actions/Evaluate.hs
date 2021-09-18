{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Evaluate
  ( evaluate,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Printer
import Language.Mimsa.Store.DepGraph
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

evaluate ::
  Text ->
  Expr Name Annotation ->
  Actions.ActionM
    ( MonoType,
      Expr Name Annotation,
      StoreExpression Annotation,
      [Graphviz],
      Expr Variable TypedAnnotation,
      Text
    )
evaluate input expr = do
  project <- Actions.getProject
  (ResolvedExpression mt se expr' scope' swaps typedExpr input') <-
    liftEither $ Actions.getTypecheckedStoreExpression input project expr
  interpretedExpr <-
    liftEither (first InterpreterErr (interpret scope' swaps expr'))
  graphviz <- Actions.graphExpression se
  Actions.appendMessage
    ( prettyPrint interpretedExpr
        <> "\n::\n"
        <> prettyPrint mt
    )
  pure (mt, interpretedExpr, se, graphviz, typedExpr, input')

---------
