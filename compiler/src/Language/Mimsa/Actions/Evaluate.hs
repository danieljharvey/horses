{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Evaluate
  ( evaluate,
  evaluateModule
  )
where

import Language.Mimsa.Modules.Uses
import Language.Mimsa.Types.Modules
import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Language.Mimsa.Actions.BindModule as Actions
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
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
    Actions.interpreter newStoreExpr -- liftEither (first InterpreterErr (interpret scope' swaps expr'))

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
-- 5. get the expression type from the module typ
-- 6. run the new expression
evaluateModule :: Text -> Expr Name Annotation -> Module Annotation -> 
  Actions.ActionM (MonoType, Expr Name Annotation, Module Annotation)
evaluateModule _input expr localModule = do
  -- work out implied imports
  let _uses = extractUses expr
  
  -- make a big module for it
  let newModule = localModule

  _typecheckedModule <- Actions.typecheckModule (prettyPrint newModule) newModule

  let exprType = MTPrim mempty MTBool

  let evaluatedExpression = expr 

  pure (exprType, evaluatedExpression, newModule)
