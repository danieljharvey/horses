{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DataTypes
  ( envWithBuiltInTypes,
    builtInTypes,
    lookupBuiltIn,
    storeDataDeclaration,
    inferDataConstructor,
    inferConstructorTypes,
    inferType,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

envWithBuiltInTypes :: Environment
envWithBuiltInTypes = Environment mempty dts mempty
  where
    makeDT (name, _) = M.singleton name (DataType name mempty mempty)
    dts = mconcat $ makeDT <$> M.toList builtInTypes

builtInTypes :: Map TyCon MonoType
builtInTypes =
  M.fromList
    [ ("String", MTPrim mempty MTString),
      ("Int", MTPrim mempty MTInt),
      ("Boolean", MTPrim mempty MTBool)
    ]

lookupBuiltIn :: TyCon -> Maybe MonoType
lookupBuiltIn name = M.lookup name builtInTypes

-- given a datatype declaration, checks it makes sense and if so,
-- add it to the Environment
storeDataDeclaration ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  DataType ->
  m Environment
storeDataDeclaration env ann dt@(DataType tyName _ _) = do
  validateDataTypeVariables dt
  validateConstructors env ann dt
  if M.member tyName (getDataTypes env)
    then throwError (DuplicateTypeDeclaration tyName)
    else
      let newEnv = Environment mempty (M.singleton tyName dt) mempty
       in pure (newEnv <> env)

errorOnBuiltIn :: (MonadError TypeError m) => Annotation -> TyCon -> m ()
errorOnBuiltIn ann tc = case lookupBuiltIn tc of
  Just _ -> throwError (InternalConstructorUsedOutsidePatternMatch ann tc)
  _ -> pure ()

-- infer the type of a data constructor
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferDataConstructor ::
  ( MonadState TypecheckState m,
    MonadError TypeError m
  ) =>
  Environment ->
  Annotation ->
  TyCon ->
  m MonoType
inferDataConstructor env ann tyCon = do
  errorOnBuiltIn ann tyCon
  dataType <- lookupConstructor env ann tyCon
  (_, allArgs) <- inferConstructorTypes env dataType
  case M.lookup tyCon allArgs of
    Just tyArg ->
      pure (constructorToType tyArg)
    Nothing -> throwError UnknownTypeError -- shouldn't happen (but will)

getVariablesForField :: Type ann -> Set Name
getVariablesForField (MTVar _ (TVName n)) = S.singleton (coerce n)
getVariablesForField (MTData _ _ fields) =
  mconcat (getVariablesForField <$> fields)
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
getVariablesForField (MTVar _ (TVNum _)) = S.empty
getVariablesForField MTPrim {} = S.empty

validateConstructors ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  DataType ->
  m ()
validateConstructors env ann (DataType _ _ constructors) = do
  traverse_
    ( \(tyCon, _) ->
        if M.member tyCon (getDataTypes env)
          then throwError (CannotUseBuiltInTypeAsConstructor ann tyCon)
          else pure ()
    )
    (M.toList constructors)

validateDataTypeVariables ::
  (MonadError TypeError m) =>
  DataType ->
  m ()
validateDataTypeVariables (DataType typeName vars constructors) =
  let requiredForCons = foldMap getVariablesForField
      requiredVars = foldMap requiredForCons constructors
      availableVars = S.fromList vars
      unavailableVars = S.filter (`S.notMember` availableVars) requiredVars
   in if S.null unavailableVars
        then pure ()
        else
          throwError $
            TypeVariablesNotInDataType typeName unavailableVars availableVars

-- infer types for data type and it's constructor in one big go
inferConstructorTypes ::
  (MonadError TypeError m, MonadState TypecheckState m) =>
  Environment ->
  DataType ->
  m (MonoType, Map TyCon TypeConstructor)
inferConstructorTypes env (DataType typeName tyVarNames constructors) = do
  tyVars <- traverse (\tyName -> (,) tyName <$> getUnknown mempty) tyVarNames
  let findType ty = case ty of
        MTData _ cn vs -> do
          vs' <- traverse findType vs
          inferType env mempty cn vs'
        MTVar _ (TVName var) ->
          case filter (\(tyName, _) -> tyName == coerce var) tyVars of
            [(_, tyFound)] -> pure tyFound
            _ ->
              throwError $
                TypeVariablesNotInDataType
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
        MTVar _ (TVNum _) ->
          throwError UnknownTypeError -- should not happen but yolo
  let inferConstructor (consName, tyArgs) = do
        tyCons <- traverse findType tyArgs
        let constructor = TypeConstructor typeName (snd <$> tyVars) tyCons
        pure $ M.singleton consName constructor
  let mtConstructors :: [(TyCon, [MonoType])]
      mtConstructors = second (($> mempty) <$>) <$> M.toList constructors
  cons' <- traverse inferConstructor mtConstructors
  let dt = MTData mempty typeName (snd <$> tyVars)
  pure (dt, mconcat cons')

-- parse a type from it's name
-- this will soon become insufficient for more complex types
inferType ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  TyCon ->
  [MonoType] ->
  m MonoType
inferType env ann tyName tyVars =
  case M.lookup tyName (getDataTypes env) of
    (Just _) -> case lookupBuiltIn tyName of
      Just mt -> pure mt
      _ -> pure (MTData mempty tyName tyVars)
    _ ->
      throwError (TypeConstructorNotInScope env ann tyName)

-----

constructorToType :: TypeConstructor -> MonoType
constructorToType (TypeConstructor typeName tyVars constructTypes) =
  foldr
    (MTFunction mempty)
    (MTData mempty typeName tyVars)
    constructTypes
