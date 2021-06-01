{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DataTypes
  ( defaultEnv,
    builtInTypes,
    lookupBuiltIn,
    storeDataDeclaration,
    inferDataConstructor,
    inferConstructorTypes,
    inferType,
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable (traverse_)
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

defaultEnv :: Substitutions -> Environment
defaultEnv (Substitutions subst) = Environment schemes dts mempty
  where
    schemes = Scheme mempty <$> subst
    makeDT (name, _) = M.singleton name (DataType name mempty mempty)
    dts = mconcat $ makeDT <$> M.toList builtInTypes

builtInTypes :: Map TyCon MonoType
builtInTypes =
  M.fromList
    [ ("String", MTPrim mempty MTString),
      ("Int", MTPrim mempty MTInt),
      ("Boolean", MTPrim mempty MTBool),
      ("StrEmpty", MTPrim mempty MTString),
      ("StrHead", MTPrim mempty MTString),
      ("ArrEmpty", MTArray mempty (MTVar mempty (TVName "a"))),
      ("ArrHead", MTArray mempty (MTVar mempty (TVName "a")))
    ]

lookupBuiltIn :: TyCon -> Maybe MonoType
lookupBuiltIn name = M.lookup name builtInTypes

-- given a datatype declaration, checks it makes sense and if so,
-- add it to the Environment
storeDataDeclaration ::
  Environment ->
  Annotation ->
  DataType Annotation ->
  TcMonad Environment
storeDataDeclaration env ann dt@(DataType tyName _ _) = do
  validateDataTypeVariables dt
  validateConstructors env ann dt
  if M.member tyName (getDataTypes env)
    then throwError (DuplicateTypeDeclaration tyName)
    else
      let newEnv = Environment mempty (M.singleton tyName dt) mempty
       in pure (newEnv <> env)

errorOnBuiltIn :: Annotation -> TyCon -> TcMonad ()
errorOnBuiltIn ann tc = case lookupBuiltIn tc of
  Just _ -> throwError (InternalConstructorUsedOutsidePatternMatch ann tc)
  _ -> pure ()

-- infer the type of a data constructor
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferDataConstructor ::
  Environment ->
  Annotation ->
  TyCon ->
  TcMonad (Substitutions, MonoType)
inferDataConstructor env ann tyCon = do
  errorOnBuiltIn ann tyCon
  dataType <- lookupConstructor env ann tyCon
  (_, allArgs) <- inferConstructorTypes env dataType
  case M.lookup tyCon allArgs of
    Just tyArg ->
      pure (mempty, constructorToType tyArg)
    Nothing -> throwError UnknownTypeError -- shouldn't happen (but will)

getVariablesForField :: MonoType -> Set Name
getVariablesForField (MTVar _ (TVName n)) = S.singleton (coerce n)
getVariablesForField (MTData _ _ fields) =
  mconcat (getVariablesForField <$> fields)
getVariablesForField (MTFunction _ a b) =
  getVariablesForField a <> getVariablesForField b
getVariablesForField (MTPair _ a b) = getVariablesForField a <> getVariablesForField b
getVariablesForField (MTRecord _ items) =
  mconcat $
    getVariablesForField <$> M.elems items
getVariablesForField _ = S.empty

validateConstructors ::
  Environment ->
  Annotation ->
  DataType Annotation ->
  TcMonad ()
validateConstructors env ann (DataType _ _ constructors) = do
  traverse_
    ( \(tyCon, _) ->
        if M.member tyCon (getDataTypes env)
          then throwError (CannotUseBuiltInTypeAsConstructor ann tyCon)
          else pure ()
    )
    (M.toList constructors)

validateDataTypeVariables :: DataType Annotation -> TcMonad ()
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
  Environment ->
  DataType Annotation ->
  TcMonad (MonoType, Map TyCon TypeConstructor)
inferConstructorTypes env (DataType typeName tyNames constructors) = do
  tyVars <- traverse (\a -> (,) a <$> getUnknown mempty) tyNames
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
        MTArray _ item -> do
          tyItems <- findType item
          pure (MTArray mempty tyItems)
        _ -> throwError UnknownTypeError -- TODO: see what this messes up, not 100% on how to deal with these
  let inferConstructor (consName, tyArgs) = do
        tyCons <- traverse findType tyArgs
        let constructor = TypeConstructor typeName (snd <$> tyVars) tyCons
        pure $ M.singleton consName constructor
  cons' <- traverse inferConstructor (M.toList constructors)
  let dt = MTData mempty typeName (snd <$> tyVars)
  pure (dt, mconcat cons')

-- parse a type from it's name
-- this will soon become insufficient for more complex types
inferType ::
  Environment ->
  Annotation ->
  TyCon ->
  [MonoType] ->
  TcMonad MonoType
inferType env ann tyName tyVars =
  case M.lookup tyName (getDataTypes env) of
    (Just _) -> case lookupBuiltIn tyName of
      Just mt -> pure mt
      _ -> pure (MTData mempty tyName tyVars)
    _ ->
      case getNativeConstructors tyName of
        Just mt -> pure mt
        _ -> throwError (TypeConstructorNotInScope env ann tyName)

-----

constructorToType :: TypeConstructor -> MonoType
constructorToType (TypeConstructor typeName tyVars constructTypes) =
  foldr (MTFunction mempty) (MTData mempty typeName tyVars) constructTypes

-------------
