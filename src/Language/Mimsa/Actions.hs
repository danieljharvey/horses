{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions
  ( evaluateText,
    evaluateStoreExpression,
    getTypecheckedStoreExpression,
    getExprPairs,
    fromItem,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Project (getCurrentBindings)
import Language.Mimsa.Store (createStoreExpression, substitute)
import Language.Mimsa.Typechecker
import Language.Mimsa.Types

----------

getType :: Swaps -> Scope -> Expr Variable -> Either Error MonoType
getType swaps scope' expr =
  first TypeErr $ startInference swaps (chainExprs expr scope')

getExprPairs :: Store -> Bindings -> [(Name, StoreExpression)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

chainExprs ::
  Expr Variable ->
  Scope ->
  Expr Variable
chainExprs expr scope = finalExpr
  where
    finalExpr =
      foldr
        (\(name, expr') a -> MyLet name expr' a)
        expr
        (M.toList . getScope $ scope)

fromItem :: Name -> StoreExpression -> ExprHash -> Project
fromItem name expr hash =
  Project
    { store = Store $ M.singleton hash expr,
      bindings = VersionedMap $ M.singleton name (pure hash),
      serverUrl = mempty,
      typeBindings = mempty
    }

evaluateStoreExpression ::
  Store ->
  StoreExpression ->
  Either Error (MonoType, StoreExpression, Expr Variable, Scope, Swaps)
evaluateStoreExpression store' storeExpr = do
  let (swaps, newExpr, scope) = substitute store' storeExpr
  exprType <- getType swaps scope newExpr
  pure (exprType, storeExpr, newExpr, scope, swaps)

getTypecheckedStoreExpression ::
  Project ->
  Expr Name ->
  Either Error (MonoType, StoreExpression, Expr Variable, Scope, Swaps)
getTypecheckedStoreExpression env expr = do
  storeExpr <- first ResolverErr $ createStoreExpression (getCurrentBindings $ bindings env) expr
  evaluateStoreExpression (store env) storeExpr

evaluateText :: Project -> Text -> Either Error (MonoType, Expr Variable, Scope, Swaps)
evaluateText env input = do
  expr <- first OtherError $ parseExpr input
  (mt, _, expr', scope', swaps) <- getTypecheckedStoreExpression env expr
  pure (mt, expr', scope', swaps)
