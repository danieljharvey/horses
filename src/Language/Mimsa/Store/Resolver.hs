{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Resolver
  ( extractVars,
    createStoreExpression,
    createTypeStoreExpression,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes (extractTypes)
import Language.Mimsa.Store.ExtractVars (extractVars)
import Language.Mimsa.Types
  ( Bindings (Bindings),
    DataType,
    Expr (MyData, MyRecord),
    ExprHash,
    Name,
    ResolverError (..),
    StoreExpression (StoreExpression),
    TyCon,
    TypeBindings (TypeBindings),
  )

--
--
-- this takes the expression, works out what it needs from it's environment
-- and wraps that up
-- this would be a good place for a simplifying step in future
-- by replacing all variables internally with a1, a2 etc, we'll get less
-- repetition, ie \x -> x and \y -> y will be the same function and thus hash

findHashInBindings :: Bindings -> Name -> Either ResolverError ExprHash
findHashInBindings (Bindings bindings') name =
  case M.lookup name bindings' of
    Just a -> Right a
    _ -> Left $ MissingBinding name (Bindings bindings')

-- given an expression, and the current environment, create a
-- store expression that captures the hashes of the functions we'll need
createStoreExpression ::
  Bindings ->
  TypeBindings ->
  Expr ann Name ->
  Either ResolverError (StoreExpression ann)
createStoreExpression bindings' tBindings expr =
  StoreExpression expr <$> findBindings bindings' expr
    <*> findTypeBindings tBindings expr

findBindings :: Bindings -> Expr ann Name -> Either ResolverError Bindings
findBindings bindings' expr = do
  let findValueHash name =
        (,) name
          <$> findHashInBindings bindings' name
  valueHashes <- traverse findValueHash (S.toList . extractVars $ expr)
  pure (Bindings (M.fromList valueHashes))

-----------
--
findHashInTypeBindings :: TypeBindings -> TyCon -> Either ResolverError ExprHash
findHashInTypeBindings (TypeBindings bindings') cName =
  case M.lookup cName bindings' of
    Just a -> Right a
    _ -> Left $ MissingType cName (TypeBindings bindings')

findTypeBindings :: TypeBindings -> Expr ann Name -> Either ResolverError TypeBindings
findTypeBindings tBindings expr = do
  let findTypeHash cName =
        (,) cName
          <$> findHashInTypeBindings tBindings cName
  hashes <- traverse findTypeHash (S.toList . extractTypes $ expr)
  pure (TypeBindings $ M.fromList hashes)

-- given a data type declaration, create a StoreExpression for it
createTypeStoreExpression ::
  (Monoid ann) =>
  TypeBindings ->
  DataType ->
  Either ResolverError (StoreExpression ann)
createTypeStoreExpression tBindings dt =
  let expr = MyData mempty dt (MyRecord mempty mempty)
   in createStoreExpression mempty tBindings expr
