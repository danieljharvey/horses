{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Actions
  ( evaluateText,
    resolveStoreExpression,
    getTypecheckedStoreExpression,
    getExprPairs,
    getTypesFromStore,
    fromItem,
    fromType,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Set (Set)
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

getType ::
  (Eq ann, Monoid ann) =>
  Swaps ->
  Scope ann ->
  Expr Variable ann ->
  Either (Error ann) MonoType
getType swaps scope' expr =
  first TypeErr $ startInference swaps (chainExprs expr scope')

getExprPairs :: Store ann -> Bindings -> [(Name, StoreExpression ann)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

getTypesFromStore :: Store ann -> TypeBindings -> Set DataType
getTypesFromStore (Store items') (TypeBindings tBindings) =
  S.fromList $ join $ do
    (_, hash) <- M.toList tBindings
    let getDt (StoreExpression expr' _ _) =
          case expr' of
            (MyData _ dt _) -> Just dt
            _ -> Nothing
    case M.lookup hash items' >>= getDt of
      Just item -> pure [item]
      _ -> pure []

chainExprs ::
  Monoid ann =>
  Expr Variable ann ->
  Scope ann ->
  Expr Variable ann
chainExprs expr scope =
  foldr
    (\(name, expr') a -> MyLet mempty name expr' a)
    expr
    (M.toList . getScope $ scope)

fromItem :: Name -> StoreExpression ann -> ExprHash -> Project ann
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

fromType :: StoreExpression ann -> ExprHash -> Project ann
fromType expr hash =
  Project
    { store = Store $ M.singleton hash expr,
      bindings = mempty,
      serverUrl = mempty,
      typeBindings = VersionedMap $ M.fromList typeList
    }
  where
    typeConsUsed =
      extractTypeDecl (storeExpression expr)
    typeList =
      (,pure hash) <$> S.toList typeConsUsed

resolveStoreExpression ::
  (Monoid ann, Eq ann) =>
  Store ann ->
  StoreExpression ann ->
  Either (Error ann) (ResolvedExpression ann)
resolveStoreExpression store' storeExpr = do
  let (SubstitutedExpression swaps newExpr scope) = substitute store' storeExpr
  exprType <- getType swaps scope newExpr
  pure (ResolvedExpression exprType storeExpr newExpr scope swaps)

getTypecheckedStoreExpression ::
  (Monoid ann, Eq ann) =>
  Project ann ->
  Expr Name ann ->
  Either (Error ann) (ResolvedExpression ann)
getTypecheckedStoreExpression env expr = do
  storeExpr <-
    first ResolverErr $
      createStoreExpression
        (getCurrentBindings $ bindings env)
        (getCurrentTypeBindings $ typeBindings env)
        expr
  resolveStoreExpression (store env) storeExpr

evaluateText ::
  (Eq ann, Monoid ann) =>
  Project ann ->
  Text ->
  Either (Error ann) (ResolvedExpression ann)
evaluateText env input = do
  expr <- first OtherError $ parseExpr input
  getTypecheckedStoreExpression env expr
