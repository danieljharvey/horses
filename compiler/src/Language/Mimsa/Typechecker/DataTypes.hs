{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.DataTypes
  ( builtInTypes,
    lookupBuiltIn,
    storeDataDeclaration,
    inferDataConstructor,
    inferConstructorTypes,
    dataTypeWithVars,
    validateDataType,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Coerce
import Data.Foldable (foldl', traverse_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.TypeUtils
import Language.Mimsa.Typechecker.BuiltIns
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Typechecker

-- on declaring a datatype, basically is it ok
validateDataType ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  DataType ->
  m ()
validateDataType _env ann dt = do
  validateDataTypeVariables ann dt
  validateConstructorsArentBuiltIns ann dt

-- given a datatype declaration, checks it makes sense and if so,
-- add it to the Environment
storeDataDeclaration ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  DataType ->
  m Environment
storeDataDeclaration env ann dt@(DataType tyName _ _) = do
  validateDataTypeVariables ann dt
  validateConstructors env ann dt
  if M.member (Nothing, tyName) (getDataTypes env)
    then throwError (DuplicateTypeDeclaration ann tyName)
    else
      let newEnv = mempty {getDataTypes = M.singleton (Nothing, tyName) dt}
       in pure (newEnv <> env)

errorOnBuiltIn :: (MonadError TypeError m) => Annotation -> TyCon -> m ()
errorOnBuiltIn ann tc = case lookupBuiltIn (coerce tc) of
  Just _ -> throwError (InternalConstructorUsedOutsidePatternMatch ann tc)
  _ -> pure ()

-- infer the type of a data constructor
-- if it has no args, it's a simple MTConstructor
-- however if it has args it becomes a MTFun from args to the MTConstructor &&
-- MTTypeApp
inferDataConstructor ::
  ( MonadState TypecheckState m,
    MonadError TypeError m
  ) =>
  Environment ->
  Annotation ->
  Maybe ModuleName ->
  TyCon ->
  m MonoType
inferDataConstructor env ann modName tyCon = do
  errorOnBuiltIn ann tyCon
  dataType <- lookupConstructor env ann modName tyCon
  (_, allArgs) <- inferConstructorTypes ann modName dataType
  case M.lookup tyCon allArgs of
    Just tyArg ->
      pure (constructorToType tyArg)
    Nothing -> throwError UnknownTypeError -- shouldn't happen (but will)

-- which vars are used in this type?
getVariablesForField :: Type ann -> Set Name
getVariablesForField (MTVar _ (TVScopedVar _ name)) = S.singleton name
getVariablesForField (MTVar _ (TVName n)) = S.singleton (coerce n)
getVariablesForField other = withMonoid getVariablesForField other

-- when adding a new datatype, check none of the constructors already exist
validateConstructors ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  DataType ->
  m ()
validateConstructors env ann (DataType _ _ constructors) = do
  traverse_
    ( \(tyCon, _) ->
        if M.member (Nothing, coerce tyCon) (getDataTypes env)
          then throwError (CannotUseBuiltInTypeAsConstructor ann (coerce tyCon))
          else pure ()
    )
    (M.toList constructors)

-- when adding a new datatype, check none of the constructors already exist
validateConstructorsArentBuiltIns ::
  (MonadError TypeError m) =>
  Annotation ->
  DataType ->
  m ()
validateConstructorsArentBuiltIns ann (DataType _ _ constructors) = do
  traverse_
    ( \(tyCon, _) ->
        case lookupBuiltIn (coerce tyCon) of
          Just _ -> throwError (CannotUseBuiltInTypeAsConstructor ann (coerce tyCon))
          _ -> pure ()
    )
    (M.toList constructors)

validateDataTypeVariables ::
  (MonadError TypeError m) =>
  Annotation ->
  DataType ->
  m ()
validateDataTypeVariables ann (DataType typeName vars constructors) =
  let requiredForCons = foldMap getVariablesForField
      requiredVars = foldMap requiredForCons constructors
      availableVars = S.fromList vars
      unavailableVars = S.filter (`S.notMember` availableVars) requiredVars
   in if S.null unavailableVars
        then pure ()
        else
          throwError $
            TypeVariablesNotInDataType ann typeName unavailableVars availableVars

-- infer types for data type and it's constructor in one big go
inferConstructorTypes ::
  (MonadError TypeError m, MonadState TypecheckState m) =>
  Annotation ->
  Maybe ModuleName ->
  DataType ->
  m (MonoType, Map TyCon TypeConstructor)
inferConstructorTypes ann modName (DataType typeName tyVarNames constructors) = do
  tyVars <- traverse (\tyName -> (,) tyName <$> getUnknown mempty) tyVarNames
  let findType ty = case ty of
        MTVar _ (TVName var) ->
          case filter (\(tyName, _) -> tyName == coerce var) tyVars of
            [(_, tyFound)] -> pure tyFound
            _ ->
              throwError $
                TypeVariablesNotInDataType
                  ann
                  typeName
                  (S.singleton (coerce var))
                  (S.fromList (fst <$> tyVars))
        MTVar _ (TVScopedVar _ var) ->
          case filter (\(tyName, _) -> tyName == coerce var) tyVars of
            [(_, tyFound)] -> pure tyFound
            _ ->
              throwError $
                TypeVariablesNotInDataType
                  ann
                  typeName
                  (S.singleton (coerce var))
                  (S.fromList (fst <$> tyVars))
        MTConstructor _ localModName tn ->
          -- if this is the datatype we are creating types for
          -- then make sure it's constructors match the namespace
          -- we are using the type in
          let newModName =
                if tn == typeName
                  then modName
                  else localModName
           in pure (MTConstructor mempty newModName tn)
        MTVar _ (TVUnificationVar _) ->
          throwError UnknownTypeError -- should not happen but yolo
        other -> bindMonoType findType other
  let inferConstructor (consName, tyArgs) = do
        tyCons <- traverse findType tyArgs
        let constructor = TypeConstructor modName typeName (snd <$> tyVars) tyCons
        pure $ M.singleton consName constructor
  let mtConstructors :: [(TyCon, [MonoType])]
      mtConstructors = second (($> mempty) <$>) <$> M.toList constructors
  cons' <- traverse inferConstructor mtConstructors
  let dt = dataTypeWithVars mempty modName typeName (snd <$> tyVars)
  pure (dt, mconcat cons')

dataTypeWithVars :: (Monoid ann) => ann -> Maybe ModuleName -> TypeName -> [Type ann] -> Type ann
dataTypeWithVars ann modName tyName =
  foldl'
    (MTTypeApp mempty)
    (MTConstructor ann modName tyName)

-----

constructorToType :: TypeConstructor -> MonoType
constructorToType (TypeConstructor modName typeName tyVars constructTypes) =
  foldr
    (MTFunction mempty)
    (dataTypeWithVars mempty modName typeName tyVars)
    constructTypes
