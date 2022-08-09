{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Actions.Modules.Check (checkModule) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Modules.RunTests as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Parse
import Language.Mimsa.Modules.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Tests
import Language.Mimsa.Types.Typechecker

-- | This is where we load a file and check that it is "OK" as such
--  so far this entails:
--  1. parsing it
--  2. ordering things
--  3. typechecking everything
--
--  so far the features in modules are
--  1. definitions of values
--  2. types of values
--  3. definitions of datatypes
--  4. exports
--  5. imports
--  6. infix
--
--  soon there will also need to be
--  1. tests
--  2. property tests
--  3. metadata / comments etc?
checkModule ::
  Map ModuleHash (Module Annotation) ->
  Text ->
  Actions.ActionM (Module (Type Annotation), ModuleTestResults)
checkModule modules input = do
  properMod <- parseModule modules input

  -- typecheck this module
  tcMods <- typecheckAllModules modules input properMod

  let (_, rootModuleHash) = serializeModule properMod

  tcMod <- lookupModule tcMods rootModuleHash

  testResults <- Actions.runModuleTests tcMod
  pure (tcMod, testResults)
