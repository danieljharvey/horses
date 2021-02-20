{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Tree
  ( doTree,
  )
where

import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (createDepGraph)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

doTree ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doTree env input expr = do
  (ResolvedExpression _ storeExpr _ _ _) <-
    mimsaFromEither $
      getTypecheckedStoreExpression input env expr
  let graph = createDepGraph "expression" (prjStore env) storeExpr
  logInfo (prettyPrint graph)
