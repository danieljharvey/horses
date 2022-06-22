{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Evaluate
  ( evaluate,
    evaluateModule,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.BindModule as Actions
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.Compile
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Prettyprinter

evaluate ::
  Text ->
  Expr Name Annotation ->
  Actions.ActionM
    ( MonoType,
      Expr Name Annotation,
      StoreExpression Annotation,
      Expr Name MonoType,
      Text
    )
evaluate input expr = do
  project <- Actions.getProject
  -- typecheck expression
  resolved <-
    Actions.typecheckExpression project input expr

  let se = reStoreExpression resolved

  -- get dependencies of StoreExpression
  depsSe <- Actions.getDepsForStoreExpression se

  -- optimise them all like a big legend
  storeExprs <- Actions.optimiseAll (fst <$> depsSe)

  -- get new root StoreExpression (it may be different due to optimisation)
  optimisedStoreExpr <- case M.lookup (getStoreExpressionHash se) storeExprs of
    Just re -> pure re
    _ -> throwError (StoreErr (CouldNotFindStoreExpression (getStoreExpressionHash se)))

  -- resolve optimised expression
  (ResolvedExpression mt newStoreExpr _expr' typedExpr input') <-
    Actions.checkStoreExpression
      (prettyPrint optimisedStoreExpr)
      project
      optimisedStoreExpr

  -- expr with types and Name
  let typedNameExpr = first fst typedExpr

  -- interpret
  interpretedExpr <-
    Actions.interpreter newStoreExpr

  -- print any warnings
  traverse_ (Actions.appendMessage . prettyPrint) (getWarnings resolved)

  -- print
  Actions.appendDocMessage
    ( group
        ( prettyDoc interpretedExpr
            <> line
            <> "::"
            <> line
            <> prettyDoc mt
        )
    )
  pure (mt, interpretedExpr, newStoreExpr, typedNameExpr, input')

---------

-- we need to bind our new expression to _something_
-- so we make a `Name` which is strictly broken, but it means
-- we won't have name collisions with a real expression
evalId :: DefIdentifier
evalId = DIName (Name "_repl")

-- given deps that this expression requires, attempt to resolve these into
-- imports we'll need from the Project environment
importsFromEntities :: Set Entity -> Actions.ActionM (Module Annotation)
importsFromEntities uses = do
  prj <- Actions.getProject
  let fromEntity = \case
        ENamespacedName modName _ ->
          case lookupModuleName prj modName of
            Right modHash -> pure $ mempty {moNamedImports = M.singleton modName modHash}
            Left found -> throwError (ProjectErr (CannotFindModuleByName modName found))
        ENamespacedType modName _ ->
          case lookupModuleName prj modName of
            Right modHash -> pure $ mempty {moNamedImports = M.singleton modName modHash}
            Left found -> throwError (ProjectErr (CannotFindModuleByName modName found))
        ENamespacedConstructor modName _ ->
          case lookupModuleName prj modName of
            Right modHash -> pure $ mempty {moNamedImports = M.singleton modName modHash}
            Left found -> throwError (ProjectErr (CannotFindModuleByName modName found))
        _ -> pure mempty
  -- check them all, combine them
  mconcat <$> traverse fromEntity (S.toList uses)

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
  Text ->
  Expr Name Annotation ->
  Module Annotation ->
  Actions.ActionM (MonoType, Expr Name Annotation, Module Annotation)
evaluateModule input expr localModule = do
  -- work out implied imports
  moduleImports <- importsFromEntities (extractUses expr)

  -- make a module for it, adding our expression as _repl
  let newModule =
        localModule
          { moExpressions =
              M.singleton evalId expr,
            moExpressionExports = S.singleton evalId
          }
          <> moduleImports

  typecheckedModules <- Actions.typecheckModules (prettyPrint newModule) newModule

  let (_, rootModuleHash) = serializeModule newModule

  -- pull root module out from pile of typechecked modules
  typecheckedModule <- case M.lookup rootModuleHash typecheckedModules of
    Just tcMod -> pure tcMod
    _ -> throwError (ModuleErr $ MissingModule rootModuleHash)

  -- compile to store expressions
  compiled <- compileModule typecheckedModules input typecheckedModule

  -- find the root StoreExpression by name
  rootStoreExpr <- case M.lookup evalId (cmExprs compiled) >>= flip M.lookup (getStore $ cmStore compiled) of
    Just se -> pure se
    _ -> error "fuck, could not find the thing we just made"

  -- unsafe, yolo
  let exprType = fromJust (lookupModuleDefType typecheckedModule evalId)

  -- need to get our new store items into the project so this works I reckon
  traverse_ Actions.appendStoreExpression (getStore $ cmStore compiled)

  -- interpret
  evaluatedExpression <-
    Actions.interpreter rootStoreExpr

  -- print
  Actions.appendDocMessage
    ( group
        ( prettyDoc evaluatedExpression
            <> line
            <> "::"
            <> line
            <> prettyDoc exprType
        )
    )

  pure (exprType, evaluatedExpression, newModule)

-- turn the module back into a bunch of StoreExpressions for interpreting etc
compileModule ::
  Map ModuleHash (Module (Type Annotation)) ->
  Text ->
  Module (Type Annotation) ->
  Actions.ActionM (CompiledModule Annotation)
compileModule typecheckedModules input inputModule = do
  modules <- prjModuleStore <$> Actions.getProject

  liftEither $ runCheck input modules (compile typecheckedModules inputModule)
