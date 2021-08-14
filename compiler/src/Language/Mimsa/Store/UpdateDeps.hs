module Language.Mimsa.Store.UpdateDeps
  ( updateStoreExpressionBindings,
    updateExprHash,
  )
where

import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

updateExprHash ::
  StoreExpression Annotation ->
  ExprHash ->
  ExprHash ->
  Bindings
updateExprHash se oldHash newHash =
  Bindings $
    (\hash -> if hash == oldHash then newHash else hash)
      <$> getBindings (storeBindings se)

updateStoreExpressionBindings ::
  Project Annotation ->
  Bindings ->
  StoreExpression Annotation ->
  Either (Error Annotation) (StoreExpression Annotation)
updateStoreExpressionBindings project newBindings se = do
  let newProject =
        project
          { prjBindings =
              bindingsToVersioned $
                combine
                  newBindings
                  (storeBindings se),
            prjTypeBindings =
              typeBindingsToVersioned
                (storeTypeBindings se)
          }
  let expr = storeExpression se
  (ResolvedExpression _ rStoreExpr _ _ _) <-
    Actions.getTypecheckedStoreExpression (prettyPrint expr) newProject expr
  pure rStoreExpr

combine :: Bindings -> Bindings -> Bindings
combine (Bindings a) (Bindings b) =
  Bindings (a <> b)
