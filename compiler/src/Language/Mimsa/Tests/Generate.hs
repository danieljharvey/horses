{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tests.Generate
  ( generateFromMonoType,
    isRecursive,
  )
where

import Data.Coerce
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.NullUnit
import Language.Mimsa.Types.Typechecker
import Test.QuickCheck

-- TODO: we'll need a namespace in the MTConstructor to make sure we generate
-- the right thing
data GenerateState = GenerateState
  { gsDataTypes :: Map (Maybe ModuleName, TypeName) DataType,
    gsDepth :: Int
  }

fromMonoType ::
  (Monoid ann) =>
  GenerateState ->
  MonoType ->
  Gen (Expr Name ann)
fromMonoType gs mt =
  case flattenRow mt of
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
    (MTRecordRow _ as _) ->
      -- as we've already run flattenRow on this to remove nested rows, assume the
      -- part on the end is just an unknown and ignore it
      MyRecord mempty <$> traverse (fromMonoType gs) as
    (MTFunction _ _from to) ->
      MyLambda mempty (Identifier mempty "a")
        <$> fromMonoType gs to
    (MTVar _ _) -> fromMonoType gs (MTPrim mempty MTBool) -- for unknowns, use bool for now
    mtTA@MTTypeApp {} -> case varsFromDataType mtTA of
      Just (_, typeName, args) -> fromType gs typeName args
      Nothing -> error "could not work out datatype"
    mtCons@MTConstructor {} -> case varsFromDataType mtCons of
      Just (_, typeName, args) -> fromType gs typeName args
      Nothing -> error "could not work out datatype"

-- | take the args for the type and apply them to the type
typeApply :: [MonoType] -> DataType -> Map TyCon [Type NullUnit]
typeApply mts (DataType _ vars constructors) =
  let subs =
        Substitutions $
          M.fromList $
            (\(k, a) -> (TVName (coerce k), a)) <$> zip vars mts
   in (fmap . fmap) (applySubst subs) constructors

fromType ::
  (Monoid ann) =>
  GenerateState ->
  TypeName ->
  [MonoType] ->
  Gen (Expr Name ann)
fromType gs typeName args = case M.lookup (Nothing, typeName) (gsDataTypes gs) of
  Just dt -> do
    let newGs = incrementDepth gs
        dtApplied = typeApply args dt
        info (tyCon, args') = do
          ( constructorWeighting newGs typeName args',
            fromConstructor newGs tyCon args'
            )
    frequency (info <$> M.toList dtApplied)
  Nothing -> error "could not find datatype"

constructorWeighting :: GenerateState -> TypeName -> [Type NullUnit] -> Int
constructorWeighting gs typeName args =
  if shouldWeStopRecursing gs
    then
      if isRecursive typeName args
        then 1 -- use recursive constructors less
        else 3 -- and non-recursive ones more
    else 1 -- equal weighting pls

-- | adjust this number to balance out good testing and interpreter crashing
shouldWeStopRecursing :: GenerateState -> Bool
shouldWeStopRecursing gs = gsDepth gs > 2

-- | To stop recursive datatypes getting ridiculous we make a depth limit
incrementDepth :: GenerateState -> GenerateState
incrementDepth (GenerateState dts depth) = GenerateState dts (depth + 1)

-- | does the type use itself?
isRecursive :: TypeName -> [Type NullUnit] -> Bool
isRecursive typeName args =
  or
    ( S.member typeName
        . extractTypenames
        <$> args
    )

fromConstructor ::
  (Monoid ann) =>
  GenerateState ->
  TyCon ->
  [Type NullUnit] ->
  Gen (Expr Name ann)
fromConstructor gs tyCon args =
  let applyArg arg mA = do
        a <- mA
        let mtArg = arg $> mempty
        MyApp mempty a <$> fromMonoType gs mtArg
   in foldr
        applyArg
        (pure (MyConstructor mempty Nothing tyCon))
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

generateFromMonoType ::
  (Monoid ann) =>
  Map (Maybe ModuleName, TypeName) DataType ->
  MonoType ->
  IO [Expr Name ann]
generateFromMonoType dataTypes mt =
  let generateState = GenerateState dataTypes 1
   in do
        let generator = fromMonoType generateState (flattenRow mt)
        sample' (resize 1000 generator)
