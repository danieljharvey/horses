module Language.Mimsa.Actions.Modules.Typecheck
  ( typecheckModules,
    typecheckModule,
  )
where

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker

typecheckModules ::
  Text ->
  Module Annotation ->
  Actions.ActionM (Map ModuleHash (Module MonoType))
typecheckModules input inputModule = do
  modules <- prjModuleStore <$> Actions.getProject

  typecheckAllModules modules input inputModule

typecheckModule :: Text -> Module Annotation -> Actions.ActionM (Module MonoType)
typecheckModule input inputModule = do
  -- typecheck it to make sure it's not silly
  typecheckedModules <-
    typecheckModules input inputModule

  let (_, rootModuleHash) = serializeModule inputModule
  case M.lookup rootModuleHash typecheckedModules of
    Just tcMod -> pure tcMod
    _ -> throwError (ModuleErr $ MissingModule rootModuleHash)
