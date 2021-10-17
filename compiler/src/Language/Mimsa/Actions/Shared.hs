{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Shared
  ( evaluateText,
    resolveStoreExpression,
    getTypecheckedStoreExpression,
    typecheckStoreExpression,
    getExprPairs,
    getTypesFromStore,
    fromItem,
    fromType,
    getTypeMap,
  )
where

import Control.Monad (join)
import Data.Bifunctor (bimap, first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Parser (parseExprAndFormatError)
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store
  ( createStoreExpression,
    substitute,
  )
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Typechecker
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.SubstitutedExpression
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

----------

getExprPairs :: Store ann -> Bindings -> [(Name, StoreExpression ann)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

-- | Typecheck everything in store and put in a map with binding name
-- | Ideally we should move this out and cache type sigs of each store
-- expression
resolvedDepsToTypeMap ::
  Store Annotation ->
  ResolvedDeps Annotation ->
  Either (Error Annotation) (Map Name MonoType)
resolvedDepsToTypeMap store' deps = do
  let resolveType se =
        resolveStoreExpression store' mempty mempty se
          >>= \(ResolvedExpression mt _ _ _ _ _ _) -> pure mt
  listItems <-
    traverse
      (\(name, (_, se)) -> (,) (NamedVar name) <$> resolveType se)
      (M.toList $ getResolvedDeps deps)
  pure (M.fromList listItems)

getTypesFromStore ::
  Store ann ->
  TypeBindings ->
  Set DataType
getTypesFromStore (Store items') (TypeBindings tBindings) =
  S.fromList $
    join $ do
      (_, hash) <- M.toList tBindings
      let getDt (StoreExpression expr' _ _) =
            case expr' of
              (MyData _ dt _) -> Just dt
              _ -> Nothing
      case M.lookup hash items' >>= getDt of
        Just item -> pure [item]
        _ -> pure []

{-
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
-}

resolveStoreExpression ::
  Store Annotation ->
  Map Variable MonoType ->
  Text ->
  StoreExpression Annotation ->
  Either (Error Annotation) (ResolvedExpression Annotation)
resolveStoreExpression store' typeMap input storeExpr = do
  let (SubstitutedExpression swaps newExpr scope) =
        substitute store' storeExpr
  let localTypeMap = getTypeMapForScopes scope
  (_, _, typedExpr, exprType) <-
    getType (localTypeMap <> typeMap) swaps scope input newExpr
  pure
    ( ResolvedExpression
        exprType
        storeExpr
        newExpr
        scope
        swaps
        typedExpr
        input
    )

-- | type map is whole project, now we need types of the specific deps for this
-- expression
getTypeMapForScopes :: Scope annotation -> Map Variable MonoType
getTypeMapForScopes = fmap g . getScope
  where
    g = const (MTPrim mempty MTInt)

-- | get the type of every binding in the project
getTypeMap :: Project Annotation -> Either (Error Annotation) (Map Variable MonoType)
getTypeMap prj = do
  resolvedDeps <-
    first
      StoreErr
      ( resolveDeps
          (prjStore prj)
          (getCurrentBindings $ prjBindings prj)
      )
  resolvedDepsToTypeMap (prjStore prj) resolvedDeps

getTypecheckedStoreExpression ::
  Text ->
  Project Annotation ->
  Expr Name Annotation ->
  Either (Error Annotation) (ResolvedExpression Annotation)
getTypecheckedStoreExpression input prj expr = do
  storeExpr <-
    first ResolverErr $
      createStoreExpression
        (getCurrentBindings $ prjBindings prj)
        (getCurrentTypeBindings $ prjTypeBindings prj)
        expr
  typeMap <- getTypeMap prj
  resolveStoreExpression (prjStore prj) typeMap input storeExpr

evaluateText ::
  Project Annotation ->
  Text ->
  Either (Error Annotation) (ResolvedExpression Annotation)
evaluateText env input = do
  expr <- first ParseError $ parseExprAndFormatError input
  getTypecheckedStoreExpression input env expr

typecheckStoreExpression ::
  Store Annotation ->
  StoreExpression Annotation ->
  Either (Error Annotation) MonoType
typecheckStoreExpression store storeExpr = do
  let project =
        Project
          store
          (bindingsToVersioned (storeBindings storeExpr))
          (typeBindingsToVersioned (storeTypeBindings storeExpr))
          mempty
  let expr = storeExpression storeExpr
  resolvedExpr <-
    getTypecheckedStoreExpression (prettyPrint expr) project expr
  pure (reMonoType resolvedExpr)

-- | Does the actual typechecking
getType ::
  Map Variable MonoType ->
  Swaps ->
  Scope Annotation ->
  Text ->
  Expr Variable Annotation ->
  Either
    (Error ann)
    ( Substitutions,
      [Constraint],
      Expr Variable MonoType,
      MonoType
    )
getType typeMap swaps _scope' source expr =
  -- TODO: add scopes to type map by typechecking them
  first
    (TypeErr source)
    ( typecheck
        typeMap
        swaps
        mempty
        expr
    )
