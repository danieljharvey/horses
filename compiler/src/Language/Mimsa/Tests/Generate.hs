{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tests.Generate (generateFromMonoType) where

import Data.Coerce
import Data.Functor
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

data GenerateState = GenerateState {gsDataTypes :: Map TyCon DataType, gsDepth :: Int}

-- | TODO: this is wildly incomplete, but let's get the mechanism working first
fromMonoType ::
  (Monoid ann) =>
  GenerateState ->
  MonoType ->
  Gen (Expr Variable ann)
fromMonoType gs mt =
  case varsFromDataType mt of
    Just (typeName, args) ->
      fromType gs typeName args
    Nothing ->
      case mt of
        (MTPrim _ prim) ->
          MyLiteral mempty <$> fromPrimitive prim
        (MTArray _ arrMt) ->
          if shouldWeStopRecursing gs
            then pure (MyArray mempty mempty)
            else MyArray mempty <$> listOf (fromMonoType gs arrMt)
        (MTPair _ a b) ->
          MyPair mempty <$> fromMonoType gs a <*> fromMonoType gs b
        (MTRecord _ as) ->
          MyRecord mempty <$> traverse (fromMonoType gs) as
        (MTFunction _ _from to) ->
          MyLambda mempty (Identifier mempty (NamedVar "a"))
            <$> fromMonoType gs to
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
  GenerateState ->
  TyCon ->
  [MonoType] ->
  Gen (Expr Variable ann)
fromType gs typeName args = case M.lookup typeName (gsDataTypes gs) of
  Just dt -> oneof (fromConstructor gs <$> M.toList (typeApply args dt))
  Nothing -> error "could not find datatype"

-- | adjust this number to balance out good testing and interpreter crashing
shouldWeStopRecursing :: GenerateState -> Bool
shouldWeStopRecursing gs = gsDepth gs > 3

-- | To stop recursive datatypes getting ridiculous we make a depth limit
incrementDepth :: GenerateState -> GenerateState
incrementDepth (GenerateState dts depth) = GenerateState dts (depth + 1)

fromConstructor ::
  (Monoid ann) =>
  GenerateState ->
  (TyCon, [Type ()]) ->
  Gen (Expr Variable ann)
fromConstructor gs (tyCon, args) =
  let newGs = incrementDepth gs
      applyArg arg mA = do
        a <- mA
        let mtArg = arg $> mempty
        MyApp mempty a <$> fromMonoType newGs mtArg
   in foldr
        applyArg
        (pure (MyConstructor mempty tyCon))
        args

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
  let generateState = GenerateState (generateTypes storeExprs) 1
   in sample' $ resize 1000 (fromMonoType generateState (flattenRow mt))
