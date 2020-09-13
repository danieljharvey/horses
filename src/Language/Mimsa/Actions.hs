{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Actions
  ( evaluateText,
    resolveStoreExpression,
    getTypecheckedStoreExpression,
    getExprPairs,
    fromItem,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Project
  ( getCurrentBindings,
    getCurrentTypeBindings,
  )
import Language.Mimsa.Store
  ( createStoreExpression,
    substitute,
  )
import Language.Mimsa.Store.ExtractTypes
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
chainExprs expr scope =
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
      typeBindings = VersionedMap $ M.fromList typeList
    }
  where
    typeConsUsed =
      extractTypeDecl (storeExpression expr)
    typeList =
      (,pure hash) <$> S.toList typeConsUsed

resolveStoreExpression ::
  Store ->
  StoreExpression ->
  Either Error ResolvedExpression
resolveStoreExpression store' storeExpr = do
  let (swaps, newExpr, scope) = substitute store' storeExpr
  exprType <- getType swaps scope newExpr
  pure (ResolvedExpression exprType storeExpr newExpr scope swaps)

getTypecheckedStoreExpression ::
  Project ->
  Expr Name ->
  Either Error ResolvedExpression
getTypecheckedStoreExpression env expr = do
  storeExpr <-
    first ResolverErr $
      createStoreExpression
        (getCurrentBindings $ bindings env)
        (getCurrentTypeBindings $ typeBindings env)
        expr
  resolveStoreExpression (store env) storeExpr

evaluateText :: Project -> Text -> Either Error ResolvedExpression
evaluateText env input = do
  expr <- first OtherError $ parseExpr input
  getTypecheckedStoreExpression env expr
