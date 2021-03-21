{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DataTypes
  ( defaultEnv,
    builtInTypes,
    storeDataDeclaration,
    inferDataConstructor,
    inferConstructorTypes,
  )
where

import Control.Monad.Except
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
      ("Unit", MTPrim mempty MTUnit)
    ]

-- given a datatype declaration, checks it makes sense and if so,
-- add it to the Environment
storeDataDeclaration ::
  Environment ->
  DataType ->
  TcMonad Environment
storeDataDeclaration env dt@(DataType tyName _ _) = do
  validateDataTypeVariables dt
  if M.member tyName (getDataTypes env)
    then throwError (DuplicateTypeDeclaration tyName)
    else
      let newEnv = Environment mempty (M.singleton tyName dt) mempty
       in pure (newEnv <> env)

-- infer the type of a data constructor
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferDataConstructor :: Environment -> Annotation -> TyCon -> TcMonad (Substitutions, MonoType)
inferDataConstructor env ann name = do
  dataType <- lookupConstructor env ann name
  (_, allArgs) <- inferConstructorTypes env dataType
  case M.lookup name allArgs of
    Just tyArg ->
      pure (mempty, constructorToType tyArg)
    Nothing -> throwError UnknownTypeError -- shouldn't happen (but will)

getVariablesForField :: Field -> Set Name
getVariablesForField (VarName n) = S.singleton n
getVariablesForField (ConsName _ fields) =
  mconcat (getVariablesForField <$> fields)
getVariablesForField (TNFunc a b) =
  getVariablesForField a <> getVariablesForField b

validateDataTypeVariables :: DataType -> TcMonad ()
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
  DataType ->
  TcMonad (MonoType, Map TyCon TypeConstructor)
inferConstructorTypes env (DataType typeName tyNames constructors) = do
  tyVars <- traverse (\a -> (,) a <$> getUnknown mempty) tyNames
  let findType ty = case ty of
        ConsName cn vs -> do
          vs' <- traverse findType vs
          inferType env mempty cn vs'
        VarName var ->
          case filter (\(tyName, _) -> tyName == var) tyVars of
            [(_, tyFound)] -> pure tyFound
            _ ->
              throwError $
                TypeVariablesNotInDataType
                  typeName
                  (S.singleton var)
                  (S.fromList (fst <$> tyVars))
        TNFunc a b -> do
          tyA <- findType a
          tyB <- findType b
          pure (MTFunction mempty tyA tyB)
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
    _ -> throwError (TypeConstructorNotInScope env ann tyName)

lookupBuiltIn :: TyCon -> Maybe MonoType
lookupBuiltIn name = M.lookup name builtInTypes

-----

constructorToType :: TypeConstructor -> MonoType
constructorToType (TypeConstructor typeName tyVars constructTypes) =
  foldr (MTFunction mempty) (MTData mempty typeName tyVars) constructTypes

-------------
