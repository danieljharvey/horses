module Language.Mimsa.Store.UpdateDeps where

import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

updateStoreExpressionBindings ::
  Project Annotation ->
  Bindings ->
  StoreExpression Annotation ->
  Either (Error Annotation) (StoreExpression Annotation)
updateStoreExpressionBindings project newBindings se = do
  let newProject = project {bindings = bindingsToVersioned $ combine newBindings (getCurrentBindings $ bindings project)}
  let expr = storeExpression se
  (ResolvedExpression _ rStoreExpr _ _ _) <-
    getTypecheckedStoreExpression (prettyPrint expr) newProject expr
  pure rStoreExpr

combine :: Bindings -> Bindings -> Bindings
combine (Bindings a) (Bindings b) =
  Bindings (a <> b)
