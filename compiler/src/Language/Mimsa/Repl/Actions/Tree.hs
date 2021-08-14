{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Tree
  ( doTree,
    doGraph,
    doProjectGraph,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Store.DepGraph
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

-- | output basic homecooked tree structure for dependencies of expr
doTree ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doTree env input expr = do
  (ResolvedExpression _ storeExpr _ _ _) <-
    mimsaFromEither $
      Actions.getTypecheckedStoreExpression input env expr
  let graph = createDepGraph "root" (prjStore env) storeExpr
  replOutput (prettyPrint graph)

-- | output dependency graph for expr in Graphviz format
doGraph ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doGraph project input expr = do
  (ResolvedExpression _ storeExpr _ _ _) <-
    mimsaFromEither $
      Actions.getTypecheckedStoreExpression input project expr
  (_, graphviz) <-
    toReplM project (Actions.graphExpression storeExpr)
  replOutput . prettyGraphviz $ graphviz

-- | create graphviz graph for all bindings in project
doProjectGraph ::
  Project Annotation -> MimsaM (Error Annotation) ()
doProjectGraph project = do
  (_, graphviz) <-
    toReplM project Actions.graphProject
  replOutput . prettyGraphviz $ graphviz
