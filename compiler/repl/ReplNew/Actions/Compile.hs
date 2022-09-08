{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ReplNew.Actions.Compile
  ( doOutputModuleJS,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import ReplNew.Helpers
import ReplNew.ReplM

doOutputModuleJS ::
  Project Annotation ->
  Maybe Backend ->
  ModuleName ->
  ReplM (Error Annotation) ()
doOutputModuleJS project maybeBackend moduleName = do
  let be = fromMaybe ESModulesJS maybeBackend
  (_, _, foundModule) <-
    replMFromEither $
      Actions.run
        project
        (Actions.lookupModuleByName moduleName)
  (_, _) <-
    toReplM project (Actions.compileModule be foundModule)
  replOutput @Text "Compilation complete!"
