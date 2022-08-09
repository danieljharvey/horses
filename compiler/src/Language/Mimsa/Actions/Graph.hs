{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Actions.Graph (graphExpression, graphProject) where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Project
import Language.Mimsa.Store.DepGraph
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleName
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

projectBindingsFromSEBindings :: Bindings -> Map (Maybe ModuleName, Name) ExprHash
projectBindingsFromSEBindings (Bindings b) = M.fromList . fmap (first (Nothing,)) . M.toList $ b

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
            (projectBindingsFromSEBindings $ getCurrentBindings $ prjBindings project)
        )
  let graphs = (\((_, k), (_, v)) -> createDepGraph k (prjStore project) v) <$> M.toList deps
  pure $ mconcat $ createGraphviz <$> graphs
