{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.BindModule (bindModule, typecheckModule) where
import Language.Mimsa.Modules.HashModule 

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.Monad
import Control.Monad.Except
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Project

typecheckModule :: Text -> Module Annotation -> 
    Actions.ActionM (Module MonoType)
typecheckModule input mod' = do
  modules <- prjModuleStore <$> Actions.getProject

  liftEither (runCheck input modules (typecheckAllModules mod'))

-- add/update a module 
bindModule ::
  Module Annotation ->
  ModuleName ->
  Text ->
  Actions.ActionM (ModuleHash, Module MonoType)
bindModule inputModule moduleName input = do
  project <- Actions.getProject
  
  -- typecheck it to make sure it's not silly
  typecheckedModule <- 
      typecheckModule input inputModule
  
  -- store the name/hash pair and save the module data in the store
  Actions.bindModuleInProject typecheckedModule moduleName
  
  -- display messages depending on whether this is new or update
  case lookupModuleName project moduleName of
    Nothing -> 
      Actions.appendMessage
        ( "Bound " <> prettyPrint moduleName <> "."
        )
    Just _ ->
        Actions.appendMessage
          ( "Updated binding of " <> prettyPrint moduleName <> "."
          )
  -- return stuff
  pure (hashModule typecheckedModule, typecheckedModule)
