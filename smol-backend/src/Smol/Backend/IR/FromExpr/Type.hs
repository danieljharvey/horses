{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Backend.IR.FromExpr.Type (fromType, typeFromEnv, fromDataTypeInMemory) where

import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Smol.Backend.IR.FromExpr.DataTypes
import qualified Smol.Backend.IR.FromExpr.Helpers as Compile
import Smol.Backend.IR.FromExpr.Types
import Smol.Backend.IR.IRExpr
import qualified Smol.Core.Types as Smol
import Smol.Typecheck.Subtype (isIntLiteral, isNatLiteral)

typeFromEnv ::
  (Show ann, MonadState (FromExprState ann) m) =>
  Map (Smol.ResolvedDep Smol.Identifier) (Smol.Type Smol.ResolvedDep ann) ->
  m IRType
typeFromEnv env = IRStruct <$> traverse fromType (M.elems env)

fromType ::
  (Show ann, MonadState (FromExprState ann) m) =>
  Smol.Type Smol.ResolvedDep ann ->
  m IRType
fromType (Smol.TPrim _ Smol.TPBool) = pure IRInt2
fromType (Smol.TPrim _ Smol.TPInt) = pure IRInt32
fromType (Smol.TPrim _ Smol.TPString) = pure (IRPointer IRInt8)
fromType (Smol.TLiteral _ lit) = pure $ fromLit lit
  where
    fromLit (Smol.TLBool _) = IRInt2
    fromLit (Smol.TLInt _) = IRInt32
    fromLit Smol.TLUnit = IRInt2 -- unit become bool?
    fromLit (Smol.TLString _as) =
      IRPointer IRInt8 -- a C string is a pointer to the char at the start of the string
fromType (Smol.TFunc _ env tArg tBody) = do
  argType <- fromType tArg
  envType <- typeFromEnv env
  irFunc <- IRFunctionType [argType, envType] <$> fromType tBody
  pure (IRStruct [IRPointer irFunc, envType])
fromType (Smol.TTuple _ tHead tTail) =
  IRStruct <$> traverse fromType ([tHead] <> NE.toList tTail)
fromType ty@Smol.TApp {} = do
  (typeName, typeArgs) <- Compile.flattenConstructorType ty
  dt <- Compile.lookupTypeName typeName
  getIrTypeForDataType dt typeArgs
fromType (Smol.TConstructor _ constructor) = do
  dt <- Compile.lookupTypeName constructor
  getIrTypeForDataType dt []
fromType (Smol.TArray _ size item) = do
  dtItem <- fromType item
  pure $ IRStruct [IRInt32, IRArray size dtItem]
fromType union | isNatLiteral union = pure IRInt32
fromType union | isIntLiteral union = pure IRInt32
fromType other =
  error $ "could not calculate IR type from smol type: " <> show other

-- get the type for a datatype
-- first we get all the args and apply them, they must not be vars
-- then we find the biggest one of the constructors and says its that
-- we'll bitcast the types (ie, coerce the type) when it comes to saving shit
-- in it
getIrTypeForDataType ::
  (Show ann, MonadState (FromExprState ann) m) =>
  Smol.DataType Smol.ResolvedDep ann ->
  [Smol.Type Smol.ResolvedDep ann] ->
  m IRType
getIrTypeForDataType dt dtArgs =
  fromDataTypeInMemory <$> getDataTypeInMemory dt dtArgs

fromDataTypeInMemory :: DataTypeInMemory -> IRType
fromDataTypeInMemory = \case
  DTEnum -> IRInt32
  DTTuple vals ->
    IRStruct (fromDataTypeInMemory <$> vals)
  DTArray size tyInner ->
    IRArray size (fromDataTypeInMemory tyInner)
  DTPrim prim -> fromTypePrim prim
  DTDataType whole _ -> fromDataTypeInMemory whole

fromTypePrim :: Smol.TypePrim -> IRType
fromTypePrim Smol.TPBool = IRInt2
fromTypePrim Smol.TPInt = IRInt32
fromTypePrim Smol.TPString = IRPointer IRInt8
