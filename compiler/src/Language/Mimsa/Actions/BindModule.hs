{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.BindModule
  ( bindModule,
    typecheckModules,
    typecheckModule,
    addBindingToModule,
  )
where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.FromParts
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Typecheck
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
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

-- add/update a module
bindModule ::
  Module Annotation ->
  ModuleName ->
  Text ->
  Actions.ActionM (ModuleHash, Module MonoType)
bindModule inputModule moduleName input = do
  project <- Actions.getProject

  -- typecheck it to make sure it's not silly
  typecheckedModule <- typecheckModule input inputModule

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
  Map ModuleHash (Module Annotation) ->
  Module MonoType ->
  ModuleItem Annotation ->
  Actions.ActionM (Module MonoType)
addBindingToModule modules mod' modItem = do
  -- add our new definition
  newModule <- addModulePart modules modItem (getAnnotationForType <$> mod')
  -- check everything still makes sense
  typecheckedModule <- typecheckModule (prettyPrint newModule) newModule
  -- output what's happened
  case getModuleItemIdentifier modItem of
    Just di ->
      Actions.appendMessage
        ("Added definition " <> prettyPrint di <> " to module")
    Nothing -> Actions.appendMessage "Module updated"

  pure typecheckedModule
