module Language.Mimsa.Actions.Optimise where

import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Transform.TrimDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Store

-- | given an expression, optimise it and create a new StoreExpression
-- | this now accepts StoreExpression instead of expression
optimise ::
  StoreExpression Annotation ->
  Actions.ActionM (StoreExpression Annotation)
optimise se = do
  let unused = findUnused (storeExpression se)
      newExpr = removeUnused (S.map fst unused) (storeExpression se)
  let newStoreExpr = trimDeps se newExpr
  Actions.appendStoreExpression newStoreExpr
  pure newStoreExpr
