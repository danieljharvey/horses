{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Backend.IR.FromExpr.DataTypes
  ( getDataTypeInMemory,
    typeToDataTypeInMemory,
    patternTypeInMemory,
    constructorTypeInMemory,
    DataTypeInMemory (..),
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup
import Data.Word (Word64)
import Smol.Backend.IR.FromExpr.Helpers
import Smol.Backend.IR.FromExpr.Types
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Subtype
import qualified Smol.Core.Typecheck.Types as Smol
import qualified Smol.Core.Types as Smol
import Smol.Core.Types.ResolvedDep

patternTypeInMemory ::
  ( Show ann,
    MonadState (FromExprState ann) m
  ) =>
  Smol.Pattern ResolvedDep (Smol.Type ResolvedDep ann) ->
  m DataTypeInMemory
patternTypeInMemory (Smol.PLiteral ty _) =
  toRepresentation ty
patternTypeInMemory (Smol.PVar ty _) =
  toRepresentation ty
patternTypeInMemory (Smol.PTuple ty _ _) =
  toRepresentation ty
patternTypeInMemory (Smol.PArray ty _ _) =
  toRepresentation ty
patternTypeInMemory (Smol.PWildcard ty) =
  toRepresentation ty
patternTypeInMemory (Smol.PConstructor ty c _) =
  snd <$> constructorTypeInMemory ty (resolveConstructor c)

constructorTypeInMemory ::
  ( MonadState (FromExprState ann) m,
    Show ann
  ) =>
  Smol.Type ResolvedDep ann ->
  Smol.Constructor ->
  m (DataTypeInMemory, DataTypeInMemory)
constructorTypeInMemory ty constructor = do
  -- there is probably a nicer version of this function that just returns
  -- the whole amd speciic types
  dtInMem <- typeToDataTypeInMemory ty

  pure $ case dtInMem of
    (Right (DTDataType whole byConstructor)) ->
      let consStruct =
            case M.lookup constructor byConstructor of
              Just found -> DTTuple $ [DTPrim Smol.TPInt] <> found
              Nothing -> error "could not find constructor in types"
       in (whole, consStruct)
    (Right DTEnum) -> (DTEnum, DTEnum)
    _ -> error "unexpected memory explanation"

typeToDataTypeInMemory ::
  ( MonadState (FromExprState ann) m,
    Show ann
  ) =>
  Smol.Type ResolvedDep ann ->
  m (Either (Smol.TCError ann) DataTypeInMemory)
typeToDataTypeInMemory ty = do
  result <- runExceptT $ flattenConstructorType ty
  case result of
    Right (typeName, typeArgs) -> do
      dt <- lookupTypeName typeName
      Right <$> getDataTypeInMemory dt typeArgs
    Left e -> pure $ Left e

-- given a datatype, and enough args to fulfil it's vars (in order)
-- plan data type as it will live in memory
-- this will fail if passed more type vars,
-- we need to get concrete, people
getDataTypeInMemory ::
  ( Show ann,
    MonadState (FromExprState ann) m
  ) =>
  Smol.DataType ResolvedDep ann ->
  [Smol.Type ResolvedDep ann] ->
  m DataTypeInMemory
getDataTypeInMemory (Smol.DataType _ [] _constructors) [] =
  pure DTEnum
getDataTypeInMemory (Smol.DataType _ dtVars constructors) args = do
  consDts <-
    traverse
      ( \cnArgs ->
          resolveDataType dtVars cnArgs args
      )
      constructors
  let arraySize =
        getMax $
          foldMap
            (Max . getSum . foldMap (Sum . howManyInts))
            (M.elems consDts)
      whole =
        DTTuple
          [ DTPrim Smol.TPInt,
            DTArray arraySize (DTPrim Smol.TPInt)
          ]
  pure $ DTDataType whole consDts

-- | very approximate way of working out how much memory to allocate
-- this can definitely be improved uponw
howManyInts :: DataTypeInMemory -> Word64
howManyInts DTEnum = 1
howManyInts (DTPrim Smol.TPInt) = 1
howManyInts (DTPrim Smol.TPBool) = 1
howManyInts (DTPrim Smol.TPString) = 1
howManyInts (DTTuple as) = getSum $ foldMap (Sum . howManyInts) as
howManyInts (DTArray size a) = size * howManyInts a
howManyInts (DTDataType whole _) = howManyInts whole -- wrong?

-- given ['e','a'], [TVar _ "a", TVar _ "e"] and [Int, Bool] return [Bool, Int]
-- putting each arg into place
resolveDataType ::
  ( Show ann,
    MonadState (FromExprState ann) m
  ) =>
  [Smol.Identifier] ->
  [Smol.Type ResolvedDep ann] ->
  [Smol.Type ResolvedDep ann] ->
  m [DataTypeInMemory]
resolveDataType dtVars constructorArgs args =
  let substitutions = zipWith Substitution (SubId . LocalDefinition <$> dtVars) args
   in traverse (toRepresentation . substituteMany substitutions) constructorArgs

toRepresentation ::
  ( Show ann,
    MonadState (FromExprState ann) m
  ) =>
  Smol.Type ResolvedDep ann ->
  m DataTypeInMemory
toRepresentation (Smol.TPrim _ prim) = pure $ DTPrim prim
toRepresentation (Smol.TLiteral _ (Smol.TLInt _)) = pure $ DTPrim Smol.TPInt
toRepresentation (Smol.TLiteral _ (Smol.TLBool _)) = pure $ DTPrim Smol.TPBool
toRepresentation (Smol.TLiteral _ (Smol.TLString _)) = pure $ DTPrim Smol.TPString
toRepresentation ty@Smol.TApp {} = do
  result <- typeToDataTypeInMemory ty
  case result of
    Right dtInMem -> pure dtInMem
    Left e -> error (show e)
toRepresentation (Smol.TTuple _ tyHead tyTail) =
  DTTuple <$> traverse toRepresentation ([tyHead] <> NE.toList tyTail)
toRepresentation (Smol.TArray _ size a) = do
  dtInner <- toRepresentation a
  pure $ DTTuple [DTPrim Smol.TPInt, DTArray size dtInner]
toRepresentation union | isNatLiteral union = pure $ DTPrim Smol.TPInt
toRepresentation union | isIntLiteral union = pure $ DTPrim Smol.TPInt
toRepresentation ty = error ("can't make rep of " <> show ty)

data DataTypeInMemory
  = DTEnum -- int
  | DTPrim Smol.TypePrim -- a primitive llvm type
  | DTTuple [DataTypeInMemory]
  | DTArray Word64 DataTypeInMemory
  | DTDataType
      { dtWhole :: DataTypeInMemory, -- a big enough allocation to fill all the constructors
        dtConstructors :: Map Smol.Constructor [DataTypeInMemory] -- the actual constructors (these don't contain the discriminator int)
      }
  deriving stock (Eq, Ord, Show)
