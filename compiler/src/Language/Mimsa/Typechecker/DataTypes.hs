{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.DataTypes
  ( builtInTypes,
    lookupBuiltIn,
    storeDataDeclaration,
    inferDataConstructor,
    inferConstructorTypes,
    inferType,
    dataTypeWithVars,
  )
where

import Language.Mimsa.Types.Modules.ModuleName
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Coerce
import Data.Foldable (foldl', traverse_)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Typechecker.BuiltIns
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

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
  if M.member (Nothing,tyName) (getDataTypes env)
    then throwError (DuplicateTypeDeclaration ann tyName)
    else
      let newEnv = mempty { getDataTypes = M.singleton (Nothing,tyName) dt } 
       in pure (newEnv <> env)

errorOnBuiltIn :: (MonadError TypeError m) => Annotation -> TyCon -> m ()
errorOnBuiltIn ann tc = case lookupBuiltIn tc of
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
  (_, allArgs) <- inferConstructorTypes ann dataType
  case M.lookup tyCon allArgs of
    Just tyArg ->
      pure (constructorToType tyArg)
    Nothing -> throwError UnknownTypeError -- shouldn't happen (but will)

-- which vars are used in this type?
getVariablesForField :: Type ann -> Set Name
getVariablesForField (MTVar _ (TVScopedVar _ name)) = S.singleton name
getVariablesForField (MTVar _ (TVName n)) = S.singleton (coerce n)
getVariablesForField (MTFunction _ a b) =
  getVariablesForField a <> getVariablesForField b
getVariablesForField (MTPair _ a b) = getVariablesForField a <> getVariablesForField b
getVariablesForField (MTRecord _ items) =
  mconcat $
    getVariablesForField <$> M.elems items
getVariablesForField (MTRecordRow _ items rest) =
  mconcat
    ( getVariablesForField <$> M.elems items
    )
    <> getVariablesForField rest
getVariablesForField (MTArray _ as) = getVariablesForField as
getVariablesForField (MTVar _ (TVUnificationVar _)) = S.empty
getVariablesForField MTPrim {} = S.empty
getVariablesForField MTConstructor {} = S.empty
getVariablesForField (MTTypeApp _ a b) = getVariablesForField a <> getVariablesForField b

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
        if M.member (Nothing,tyCon) (getDataTypes env)
          then throwError (CannotUseBuiltInTypeAsConstructor ann tyCon)
          else pure ()
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
  DataType ->
  m (MonoType, Map TyCon TypeConstructor)
inferConstructorTypes ann (DataType typeName tyVarNames constructors) = do
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
        MTFunction _ a b -> do
          tyA <- findType a
          tyB <- findType b
          pure (MTFunction mempty tyA tyB)
        MTPair _ a b -> do
          tyA <- findType a
          tyB <- findType b
          pure (MTPair mempty tyA tyB)
        tyPrim@MTPrim {} -> pure tyPrim
        tyCon@MTConstructor {} -> pure tyCon
        MTRecord _ items -> do
          tyItems <- traverse findType items
          pure (MTRecord mempty tyItems)
        MTRecordRow _ items rest -> do
          tyItems <- traverse findType items
          MTRecordRow mempty tyItems
            <$> findType rest
        MTArray _ item -> do
          tyItems <- findType item
          pure (MTArray mempty tyItems)
        MTTypeApp _ func arg ->
          MTTypeApp mempty <$> findType func <*> findType arg
        MTVar _ (TVUnificationVar _) ->
          throwError UnknownTypeError -- should not happen but yolo
  let inferConstructor (consName, tyArgs) = do
        tyCons <- traverse findType tyArgs
        let constructor = TypeConstructor typeName (snd <$> tyVars) tyCons
        pure $ M.singleton consName constructor
  let mtConstructors :: [(TyCon, [MonoType])]
      mtConstructors = second (($> mempty) <$>) <$> M.toList constructors
  cons' <- traverse inferConstructor mtConstructors
  let dt = dataTypeWithVars mempty typeName (snd <$> tyVars)
  pure (dt, mconcat cons')

-- parse a type from it's name
-- this will soon become insufficient for more complex types
inferType ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
    Maybe ModuleName ->
  TyCon ->
  [MonoType] ->
  m MonoType
inferType env ann modName tyName tyVars =
  case M.lookup (modName,tyName) (getDataTypes env) of
    (Just _) -> case lookupBuiltIn tyName of
      Just mt -> pure mt
      _ -> pure (dataTypeWithVars mempty tyName tyVars)
    _ ->
      throwError (TypeConstructorNotInScope env ann modName tyName)

dataTypeWithVars :: (Monoid ann) => ann -> TyCon -> [Type ann] -> Type ann
dataTypeWithVars ann tyName =
  foldl'
    (MTTypeApp mempty)
    (MTConstructor ann tyName)

-----

constructorToType :: TypeConstructor -> MonoType
constructorToType (TypeConstructor typeName tyVars constructTypes) =
  foldr
    (MTFunction mempty)
    (dataTypeWithVars mempty typeName tyVars)
    constructTypes
