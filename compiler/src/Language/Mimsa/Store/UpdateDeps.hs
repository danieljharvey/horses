module Language.Mimsa.Store.UpdateDeps
  ( updateStoreExpressionBindings,
    updateExprHash,
  )
where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

updateExprHash ::
  StoreExpression Annotation ->
  ExprHash ->
  ExprHash ->
  Map (Maybe ModuleName, Name) ExprHash
updateExprHash se oldHash newHash =
  (\hash -> if hash == oldHash then newHash else hash)
    <$> storeBindings se

updateStoreExpressionBindings ::
  Project Annotation ->
  Map (Maybe ModuleName, Name) ExprHash ->
  StoreExpression Annotation ->
  Either (Error Annotation) (StoreExpression Annotation)
updateStoreExpressionBindings project newBindings se = do
  let newProject =
        project
          { prjBindings =
              toVersioned
                ( binOffModule $
                    newBindings
                      <> storeBindings se
                ),
            prjTypeBindings =
              typeBindingsToVersioned
                (storeTypeBindings se)
          }
  let expr = storeExpression se
  (_, _, resolvedExpr) <-
    Actions.run
      newProject
      ( Actions.typecheckExpression newProject (prettyPrint expr) expr
      )
  pure (reStoreExpression resolvedExpr)

binOffModule :: (Ord k2) => Map (k1, k2) a -> Map k2 a
binOffModule = M.fromList . fmap (first snd) . M.toList
