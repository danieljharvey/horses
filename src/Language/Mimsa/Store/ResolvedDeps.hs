module Language.Mimsa.Store.ResolvedDeps
  ( resolveDeps,
    recursiveResolve,
    recursiveResolveSE,
  )
where

import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import qualified Data.Map as M
import Language.Mimsa.Types

-- we spend so much time passing the whole store around to match hashes
-- lets create one way of resolving a pile of them and be done with it

resolveDeps :: Store -> Bindings -> Either [Name] ResolvedDeps
resolveDeps (Store items) (Bindings bindings') =
  case partitionEithers foundItems of
    ([], found) -> Right (ResolvedDeps (M.fromList found))
    (fails, _) -> Left fails
  where
    foundItems =
      ( \(name, hash) -> case M.lookup hash items of
          Just storeExpr' -> Right (name, (hash, storeExpr'))
          Nothing -> Left name
      )
        <$> M.toList bindings'

recursiveResolve :: Store -> StoreExpression -> Either [Name] [(Name, Expr Name)]
recursiveResolve store' storeExpr = do
  (ResolvedDeps deps) <- resolveDeps store' (storeBindings storeExpr)
  let storeExprs :: [(Name, StoreExpression)]
      storeExprs = (\(name, (_, se)) -> (name, se)) <$> M.toList deps
  subExprs <- traverse (recursiveResolve store') (snd <$> storeExprs)
  pure $ mconcat subExprs <> (second storeExpression <$> storeExprs)

recursiveResolveSE :: Store -> StoreExpression -> Either [Name] [StoreExpression]
recursiveResolveSE store' storeExpr = do
  (ResolvedDeps deps) <- resolveDeps store' (storeBindings storeExpr)
  let storeExprs :: [(Name, StoreExpression)]
      storeExprs = (\(name, (_, se)) -> (name, se)) <$> M.toList deps
  subExprs <- traverse (recursiveResolveSE store') (snd <$> storeExprs)
  pure $ mconcat subExprs <> (snd <$> storeExprs)
