module Language.Mimsa.Tui.Evaluate (getExpressionForBinding) where

import qualified Brick.Widgets.List as L
import qualified Data.Map as M
import Language.Mimsa.Actions (resolveStoreExpression)
import Language.Mimsa.Store
import Language.Mimsa.Types

evaluateStoreExprToInfo ::
  (Eq ann, Monoid ann) =>
  Store ann ->
  StoreExpression ann ->
  Maybe (MonoType, Expr Name ann)
evaluateStoreExprToInfo store' storeExpr =
  case resolveStoreExpression store' storeExpr of
    Right (ResolvedExpression mt _ _ _ _) -> Just (mt, storeExpression storeExpr)
    _ -> Nothing

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

getExpressionForBinding ::
  (Eq ann, Monoid ann) =>
  Store ann ->
  ResolvedDeps ann ->
  L.List () Name ->
  Maybe (ExpressionInfo ann)
getExpressionForBinding store' (ResolvedDeps deps) l = do
  (_, name) <- L.listSelectedElement l
  (_, storeExpr') <- M.lookup name deps
  subDeps <- hush $ resolveDeps store' (storeBindings storeExpr')
  let toInfo (mt, expr) = ExpressionInfo mt expr name subDeps
  toInfo <$> evaluateStoreExprToInfo store' storeExpr'
