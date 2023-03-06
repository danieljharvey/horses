module Language.Mimsa.Actions.Helpers.LookupExpression
  ( lookupModule,
    lookupModuleByName,
  )
where

import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Core
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project

lookupModuleByName :: ModuleName -> Actions.ActionM (Module Annotation)
lookupModuleByName modName = do
  project <- Actions.getProject
  case lookupModuleName project modName of
    Right modHash -> lookupModule modHash
    Left found -> throwError (ProjectErr (CannotFindModuleByName modName found))

lookupModule ::
  ModuleHash ->
  Actions.ActionM (Module Annotation)
lookupModule modHash = do
  project <- Actions.getProject
  case M.lookup modHash (prjModuleStore project) of
    Just mod' -> pure mod'
    Nothing -> throwError (StoreErr (CouldNotFindModule modHash))
