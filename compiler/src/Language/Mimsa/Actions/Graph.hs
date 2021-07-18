{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Graph (graphExpression, graphProject) where

import Control.Monad.Except
import Data.Bifunctor (first)
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Project
import Language.Mimsa.Store.DepGraph
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

-- | create graphviz file for store expression
graphExpression ::
  StoreExpression Annotation ->
  Actions.ActionM [Graphviz]
graphExpression storeExpr = do
  project <- Actions.getProject
  let graph = createDepGraph "root" (prjStore project) storeExpr
  pure (createGraphviz graph)

-- | create Graphviz for entire project
graphProject :: Actions.ActionM [Graphviz]
graphProject = do
  project <- Actions.getProject
  (ResolvedDeps deps) <-
    liftEither $
      first
        StoreErr
        ( resolveDeps
            (prjStore project)
            (getCurrentBindings $ prjBindings project)
        )
  let graphs = (\(k, (_, v)) -> createDepGraph k (prjStore project) v) <$> M.toList deps
  pure $ mconcat $ createGraphviz <$> graphs
