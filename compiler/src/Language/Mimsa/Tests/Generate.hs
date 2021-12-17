{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tests.Generate (generateFromMonoType) where

import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Text as T
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Test.QuickCheck

-- | TODO: this is wildly incomplete, but let's get the mechanism working first
fromMonoType ::
  (Monoid ann) =>
  Map TyCon DataType ->
  MonoType ->
  Gen (Expr Variable ann)
fromMonoType dts mt =
  case varsFromDataType mt of
    Just (typeName, args) ->
      fromType dts typeName args
    Nothing ->
      case mt of
        (MTPrim _ prim) ->
          MyLiteral mempty <$> fromPrimitive prim
        (MTArray _ arrMt) ->
          MyArray mempty <$> listOf (fromMonoType dts arrMt)
        (MTPair _ a b) ->
          MyPair mempty <$> fromMonoType dts a <*> fromMonoType dts b
        (MTRecord _ as) ->
          MyRecord mempty <$> traverse (fromMonoType dts) as
        (MTFunction _ _from to) ->
          MyLambda mempty (Identifier mempty (NamedVar "a"))
            <$> fromMonoType dts to
        _ -> undefined

-- | take the args for the type and apply them to the type
typeApply :: [MonoType] -> DataType -> Map TyCon [Type ()]
typeApply mts (DataType _ vars constructors) =
  let subs =
        Substitutions $
          M.fromList $
            (\(k, a) -> (TVName Nothing (coerce k), a)) <$> zip vars mts
   in (fmap . fmap) (applySubst subs) constructors

fromType ::
  (Monoid ann) =>
  Map TyCon DataType ->
  TyCon ->
  [MonoType] ->
  Gen (Expr Variable ann)
fromType dts typeName args = case M.lookup typeName dts of
  Just dt -> oneof (fromConstructor <$> M.toList (typeApply args dt))
  Nothing -> error "could not find datatype"

fromConstructor ::
  (Monoid ann) =>
  (TyCon, [Type ()]) ->
  Gen (Expr Variable ann)
fromConstructor (tyCon, _args) =
  pure (MyConstructor mempty tyCon)

fromPrimitive :: Primitive -> Gen Literal
fromPrimitive MTBool =
  MyBool <$> chooseAny
fromPrimitive MTInt =
  MyInt <$> chooseAny
fromPrimitive MTString =
  -- TODO: are these valid StringType values? probably not, may be a beef when
  -- we come to Interpret these in tests
  MyString . StringType . T.pack <$> listOf chooseAny

generateTypes ::
  Set (StoreExpression Annotation) ->
  Map TyCon DataType
generateTypes storeExprs = getDataTypes $ createEnv mempty storeExprs

generateFromMonoType ::
  (Monoid ann) =>
  Set (StoreExpression Annotation) ->
  MonoType ->
  IO [Expr Variable ann]
generateFromMonoType storeExprs mt =
  sample' (fromMonoType (generateTypes storeExprs) (flattenRow mt))
