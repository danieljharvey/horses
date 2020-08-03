module Language.Mimsa.Tui.Evaluate (getExpressionForBinding) where

import qualified Brick.Widgets.List as L
import qualified Data.Map as M
import Language.Mimsa.Actions (evaluateStoreExpression)
import Language.Mimsa.Store
import Language.Mimsa.Types

evaluateStoreExprToInfo ::
  Store ->
  StoreExpression ->
  Maybe (MonoType, Expr Name)
evaluateStoreExprToInfo store' storeExpr =
  case evaluateStoreExpression store' storeExpr of
    Right (mt, _, _, _) -> Just (mt, storeExpression storeExpr)
    _ -> Nothing

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

getExpressionForBinding ::
  Store ->
  ResolvedDeps ->
  L.List () Name ->
  Maybe ExpressionInfo
getExpressionForBinding store' (ResolvedDeps deps) l = do
  (_, name) <- L.listSelectedElement l
  (_, storeExpr') <- M.lookup name deps
  subDeps <- hush $ resolveDeps store' (storeBindings storeExpr')
  let toInfo (mt, expr) = ExpressionInfo mt expr name subDeps
  toInfo <$> evaluateStoreExprToInfo store' storeExpr'
