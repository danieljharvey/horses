{-# LANGUAGE OverloadedStrings #-}

module ReplNew.Actions.Tree
  ( doTree,
    doGraph,
    doProjectGraph,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store.DepGraph
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import ReplNew.Helpers
import ReplNew.ReplM

-- | output basic homecooked tree structure for dependencies of expr
doTree ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM (Error Annotation) ()
doTree project input expr = do
  (_, _, resolvedExpr) <-
    replMFromEither $
      Actions.run project (Actions.typecheckExpression project input expr)
  let graph = createDepGraph "root" (prjStore project) (reStoreExpression resolvedExpr)
  replOutput (prettyPrint graph)

-- | output dependency graph for expr in Graphviz format
doGraph ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM (Error Annotation) ()
doGraph project input expr = do
  (_, _, resolvedExpr) <-
    replMFromEither $
      Actions.run
        project
        ( Actions.typecheckExpression project input expr
        )
  (_, graphviz) <-
    toReplM project (Actions.graphExpression (reStoreExpression resolvedExpr))
  replOutput . prettyGraphviz $ graphviz

-- | create graphviz graph for all bindings in project
doProjectGraph ::
  Project Annotation -> ReplM (Error Annotation) ()
doProjectGraph project = do
  (_, graphviz) <-
    toReplM project Actions.graphProject
  replOutput . prettyGraphviz $ graphviz
