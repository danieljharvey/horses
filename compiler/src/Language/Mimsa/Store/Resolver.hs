module Language.Mimsa.Store.Resolver
  ( extractVars,
    createStoreExpression,
    createTypeStoreExpression,
  )
where

import Data.Coerce
import Data.Either
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes (extractTypes)
import Language.Mimsa.Store.ExtractVars (extractVars)
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

-- this takes the expression, works out what it needs from it's environment
-- and wraps that up
-- this would be a good place for a simplifying step in future
-- by replacing all variables internally with a1, a2 etc, we'll get less
-- repetition, ie \x -> x and \y -> y will be the same function and thus hash

-- basically, (global Bindings, Expression) -> StoreExpression

findHashInBindings :: Bindings -> Name -> Either ResolverError ExprHash
findHashInBindings (Bindings bindings') name =
  case M.lookup name bindings' of
    Just a -> Right a
    _ -> Left $ MissingBinding name (Bindings bindings')

-- given an expression, and the current environment, create a
-- store expression that captures the hashes of the functions we'll need
createStoreExpression ::
  (Eq ann, Monoid ann) =>
  Bindings ->
  TypeBindings ->
  Expr Name ann ->
  Either ResolverError (StoreExpression ann)
createStoreExpression bindings' tBindings expr =
  StoreExpression expr
    <$> findBindings bindings' expr
    <*> findTypeBindings tBindings expr

findBindings ::
  (Eq ann, Monoid ann) =>
  Bindings ->
  Expr Name ann ->
  Either ResolverError Bindings
findBindings bindings' expr = do
  let findValueHash name =
        (,) name
          <$> findHashInBindings bindings' name
  valueHashes <- traverse findValueHash (S.toList . extractVars $ expr)
  pure (Bindings (M.fromList valueHashes))

-----------

typeNameIsBuiltIn :: TypeName -> Bool
typeNameIsBuiltIn = isJust . lookupBuiltIn

tyConIsBuiltIn :: TyCon -> Bool
tyConIsBuiltIn = isJust . lookupBuiltIn . coerce

findConstructorHashInTypeBindings ::
  TypeBindings ->
  TyCon ->
  Either ResolverError (Maybe ExprHash)
findConstructorHashInTypeBindings tb@(TypeBindings _tnBindings tcBindings) tyCon =
  if tyConIsBuiltIn tyCon
    then Right Nothing
    else case M.lookup tyCon tcBindings of
      Just a -> Right (Just a)
      _ -> Left $ MissingTyCon tyCon tb

findTypeNameHashInTypeBindings ::
  TypeBindings ->
  TypeName ->
  Either ResolverError (Maybe ExprHash)
findTypeNameHashInTypeBindings tb@(TypeBindings tnBindings _tcBindings) typeName =
  if typeNameIsBuiltIn typeName
    then Right Nothing
    else case M.lookup typeName tnBindings of
      Just a -> Right (Just a)
      _ -> Left $ MissingTypeName typeName tb

findTypeBindings ::
  TypeBindings ->
  Expr Name ann ->
  Either ResolverError TypeBindings
findTypeBindings tBindings expr = do
  let findTyConHash tyCon = do
        maybeHash <- findConstructorHashInTypeBindings tBindings tyCon
        pure $ (,) tyCon <$> maybeHash
  let findTypeNameHash typeName = do
        maybeHash <- findTypeNameHashInTypeBindings tBindings typeName
        pure $ (,) typeName <$> maybeHash

  let (tyCons, typeNames) =
        partitionEithers $
          S.toList $ extractTypes expr
  tnHashes <- traverse findTypeNameHash typeNames
  tcHashes <- traverse findTyConHash tyCons
  pure
    ( TypeBindings
        (M.fromList (catMaybes tnHashes))
        (M.fromList (catMaybes tcHashes))
    )

-- given a data type declaration, create a StoreExpression for it
createTypeStoreExpression ::
  (Eq ann, Monoid ann) =>
  TypeBindings ->
  DataType ->
  Either ResolverError (StoreExpression ann)
createTypeStoreExpression tBindings dt =
  let expr = MyData mempty dt (MyRecord mempty mempty)
   in createStoreExpression mempty tBindings expr
