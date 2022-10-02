
module Language.Mimsa.Actions.Modules.Evaluate
  ( evaluateModule,
  )
where

import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Modules.ToStoreExpressions as Actions
import qualified Language.Mimsa.Actions.Modules.Typecheck as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.ToStoreExprs
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import qualified Language.Mimsa.Actions.Modules.Imports as Actions

-- when we evaluate an expression, really we are adding it to an open module
-- then evaluating the expression in the context of that module
-- this means we can bind successive values
-- we should probably stop users adding the same type/value twice so we don't
-- have to deal with all the confusion
--
-- 1. work out what stuff the expression uses
-- 2. turns those into a module (ie, implied imports, new defs etc)
-- 3. combine that with the local module
-- 4. typecheck it
-- 5. get the expression type from the module type
-- 6. compile into store expressions
-- 6. interpret store expressions as normal
evaluateModule ::
  Expr Name Annotation ->
  Module Annotation ->
  Actions.ActionM (MonoType, Expr Name Annotation, Module Annotation)
evaluateModule expr localModule = do
  -- work out implied imports
  moduleImports <-
    Actions.importsFromEntities
      ( extractUses expr
          <> Actions.entitiesFromModule localModule
      )

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
  typecheckedModule <- Actions.typecheckModule (prettyPrint newModule) newModule

  -- compile to store expressions
  compiled <- Actions.toStoreExpressions typecheckedModule

  -- find the root StoreExpression by name
  rootStoreExpr <- Actions.lookupByName compiled Actions.evalId

  -- unsafe, yolo
  let exprType = fromJust (lookupModuleDefType typecheckedModule Actions.evalId)

  -- need to get our new store items into the project so this works I reckon
  traverse_
    (Actions.appendStoreExpression . fmap getAnnotationForType)
    (getStore $ cmStore compiled)

  -- interpret
  evaluatedExpression <-
    Actions.interpreter (getAnnotationForType <$> rootStoreExpr)

  pure (exprType, evaluatedExpression, newModule)
