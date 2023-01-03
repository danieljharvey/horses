module Language.Mimsa.Actions.Modules.Typecheck
  ( typecheckModules,
    typecheckModule,
    typecheckExpression,
  )
where

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Modules.Imports as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Typecheck
import Language.Mimsa.Core
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
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

-- | typecheck a single expression in the context of modules
typecheckExpression ::
  Expr Name Annotation ->
  Module Annotation ->
  Actions.ActionM (Expr Name MonoType)
typecheckExpression expr localModule = do
  -- work out implied imports
  moduleImports <- Actions.findUsesInProject expr localModule

  -- make a module for it, adding our expression as _repl
  let newModule =
        localModule
          <> mempty
            { moExpressions =
                M.singleton Actions.evalId expr,
              moExpressionExports = S.singleton Actions.evalId
            }
          <> moduleImports

  -- typecheck it
  typecheckedModule <- typecheckModule (prettyPrint newModule) newModule

  -- unsafe, yolo
  pure $ fromJust (lookupModuleDef typecheckedModule Actions.evalId)
