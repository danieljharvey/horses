{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Modules.Bind
  ( bindModule,
    addBindingToModule,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Modules.RunTests as Actions
import qualified Language.Mimsa.Actions.Modules.Typecheck as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.FromParts
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Core
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Core
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Tests
import Language.Mimsa.Types.Typechecker

-- add/update a module
bindModule ::
  Module Annotation ->
  ModuleName ->
  Text ->
  Actions.ActionM (ModuleHash, Module MonoType)
bindModule inputModule moduleName input = do
  project <- Actions.getProject

  -- typecheck it to make sure it's not silly
  typecheckedModule <- Actions.typecheckModule input inputModule

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
  Actions.ActionM (Module MonoType, ModuleTestResults)
addBindingToModule modules mod' modItem = do
  -- add our new definition
  newModule <- addModulePart modules modItem (getAnnotationForType <$> mod')
  -- check everything still makes sense
  typecheckedModule <- Actions.typecheckModule (prettyPrint newModule) newModule
  -- run tests
  testResults <- Actions.runModuleTests typecheckedModule
  -- output what's happened
  case getModuleItemIdentifier modItem of
    Just di ->
      Actions.appendMessage
        ("Added definition " <> prettyPrint di <> " to module")
    Nothing -> Actions.appendMessage "Module updated"

  pure (typecheckedModule, testResults)
