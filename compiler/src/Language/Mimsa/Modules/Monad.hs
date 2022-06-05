{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Modules.Monad (CheckM (..), CheckEnv (..), runCheck,  getStoredInput, 
    lookupModule,lookupModuleDep,lookupModuleType, errorIfExpressionAlreadyDefined,checkDataType,errorIfImportAlreadyDefined, errorIfTypeImportAlreadyDefined) where

import Data.Foldable
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Data.Coerce
import qualified Data.Set as S
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Typechecker


-- this is where we keep all the modules we need to do things
data CheckEnv ann = CheckEnv
  { ceModules :: Map ModuleHash (Module ann),
    ceInput :: Text
  }

newtype CheckM a = CheckM
  { runCheckM ::
      ExceptT
        (Error Annotation)
        ( Reader (CheckEnv Annotation)
        )
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError (Error Annotation),
      MonadReader (CheckEnv Annotation)
    )

runCheck :: Text -> Map ModuleHash (Module Annotation) -> CheckM a -> Either (Error Annotation) a
runCheck input modules comp = runReader (runExceptT (runCheckM comp)) initialEnv
  where
    initialEnv =
      CheckEnv
        { ceModules = modules, 
          ceInput = input 
        }

getStoredInput :: CheckM Text
getStoredInput = asks ceInput


lookupModule :: ModuleHash -> CheckM (Module Annotation)
lookupModule modHash = do
  mods <- asks ceModules
  case M.lookup modHash mods of
    Just foundModule -> pure foundModule
    _ -> throwError (ModuleErr (MissingModule modHash))

lookupModuleDep ::
  Map ModuleHash (Module (Type Annotation)) ->
  DefIdentifier ->
  ModuleHash ->
  CheckM (Expr Name (Type Annotation))
lookupModuleDep typecheckedModules def modHash = do
  case M.lookup modHash typecheckedModules of
    Just mod' ->
      case M.lookup def (moExpressions mod') of
        Just expr -> pure expr
        _ -> throwError (ModuleErr (MissingModuleDep def modHash))
    _ -> throwError (ModuleErr (MissingModule modHash))

lookupModuleType ::
  Map ModuleHash (Module (Type Annotation)) ->
  TypeName ->
  ModuleHash ->
  CheckM DataType
lookupModuleType typecheckedModules typeName modHash = do
  case M.lookup modHash typecheckedModules of
    Just mod' ->
      case M.lookup typeName (moDataTypes mod') of
        Just dt -> pure dt
        _ -> throwError (ModuleErr (MissingModuleTypeDep typeName modHash))
    _ -> throwError (ModuleErr (MissingModule modHash))


errorIfExpressionAlreadyDefined :: Module ann -> DefIdentifier -> CheckM ()
errorIfExpressionAlreadyDefined mod' def =
  if M.member def (moExpressions mod')
    || M.member def (moExpressionImports mod')
    then throwError (ModuleErr $ DuplicateDefinition def)
    else pure ()

checkDataType :: Module ann -> DataType -> CheckM ()
checkDataType mod' (DataType typeName _ constructors) = do
  errorIfTypeAlreadyDefined mod' (coerce typeName)
  traverse_ (errorIfConstructorAlreadyDefined mod') (M.keys constructors)

errorIfTypeAlreadyDefined :: Module ann -> TypeName -> CheckM ()
errorIfTypeAlreadyDefined mod' typeName =
  if M.member typeName (moDataTypes mod')
    || M.member typeName (moDataTypeImports mod')
    then throwError (ModuleErr $ DuplicateTypeName typeName)
    else pure ()

errorIfConstructorAlreadyDefined :: Module ann -> TyCon -> CheckM ()
errorIfConstructorAlreadyDefined mod' tyCon =
  let allCons = mconcat (M.keysSet . dtConstructors <$> M.elems (moDataTypes mod'))
   in if S.member tyCon allCons
        then throwError (ModuleErr $ DuplicateConstructor tyCon)
        else pure ()

errorIfImportAlreadyDefined :: Module ann -> DefIdentifier -> ModuleHash -> CheckM ()
errorIfImportAlreadyDefined mod' def moduleHash =
  if M.member def (moExpressions mod')
    || M.member def (moExpressionImports mod')
    then throwError (ModuleErr $ DefinitionConflictsWithImport def moduleHash)
    else pure ()

errorIfTypeImportAlreadyDefined :: Module ann -> TypeName -> ModuleHash -> CheckM ()
errorIfTypeImportAlreadyDefined mod' typeName moduleHash =
  if M.member typeName (moDataTypes mod')
    || M.member typeName (moDataTypeImports mod')
    then throwError (ModuleErr $ TypeConflictsWithImport typeName moduleHash)
    else pure ()


