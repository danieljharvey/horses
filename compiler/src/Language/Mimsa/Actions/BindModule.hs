{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.BindModule
  ( bindModule,
    typecheckModules,
    addBindingToModule,
  )
where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Typecheck
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker

typecheckModules ::
  Text ->
  Module Annotation ->
  Actions.ActionM (Map ModuleHash (Module MonoType))
typecheckModules input inputModule = do
  modules <- prjModuleStore <$> Actions.getProject

  liftEither $
    runCheck
      input
      modules
      (typecheckAllModules inputModule)

-- add/update a module
bindModule ::
  Module Annotation ->
  ModuleName ->
  Text ->
  Actions.ActionM (ModuleHash, Module MonoType)
bindModule inputModule moduleName input = do
  project <- Actions.getProject

  -- typecheck it to make sure it's not silly
  typecheckedModules <-
    typecheckModules input inputModule

  let (_, rootModuleHash) = serializeModule inputModule
  typecheckedModule <- case M.lookup rootModuleHash typecheckedModules of
    Just tcMod -> pure tcMod
    _ -> throwError (ModuleErr $ MissingModule rootModuleHash)

  -- store the name/hash pair and save the module data in the store
  Actions.bindModuleInProject typecheckedModule moduleName

  -- display messages depending on whether this is new or update
  case lookupModuleName project moduleName of
    Right _ ->
      Actions.appendMessage
        ( "Updated binding of " <> prettyPrint moduleName <> "."
        )
    _ ->
      Actions.appendMessage
        ( "Bound " <> prettyPrint moduleName <> "."
        )

  -- return stuff
  pure (snd (serializeModule typecheckedModule), typecheckedModule)

addBindingToModule ::
  Module MonoType ->
  ModuleItem Annotation ->
  Text ->
  Actions.ActionM (Module MonoType)
addBindingToModule _mod' _modItem _input = do
  error "fuck"
