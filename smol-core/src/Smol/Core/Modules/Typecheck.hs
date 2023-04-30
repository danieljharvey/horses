{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Smol.Core.Modules.Typecheck (typecheckModule) where

-- import Smol.Core.Types.Module.ModuleName

import qualified Builder as Build
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
-- import Smol.Core.Modules.HashModule
-- import Smol.Core.Modules.Monad

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Smol.Core
import Smol.Core.Modules.Check (filterNameDefs)
import Smol.Core.Modules.Dependencies
import Smol.Core.Modules.ModuleError
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleHash

--- typecheck a single module
typecheckModule ::
  (MonadError ModuleError m) =>
  Map ModuleHash (Module ResolvedDep (Type ResolvedDep Annotation)) ->
  Text ->
  Module ResolvedDep Annotation ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
typecheckModule _typecheckedDeps input inputModule = do
  let inputWithDepsAndName = M.mapWithKey (,) (DTExpr <$> moExpressions inputModule)

  let stInputs =
        ( \(name, expr) ->
            Build.Plan
              { Build.jbDeps = mempty, -- deps,
                Build.jbInput = (name, expr)
              }
        )
          <$> inputWithDepsAndName

  let state =
        Build.State
          { Build.stInputs = stInputs,
            Build.stOutputs = mempty
          }
  -- go!
  typecheckedDefs <-
    Build.stOutputs
      <$> Build.doJobs (typecheckOneDef input inputModule) state

  -- replace input module with typechecked versions
  pure $
    inputModule
      { moExpressions = filterExprs typecheckedDefs,
        moDataTypes = mempty
      }

-- given types for other required definition, typecheck a definition
typecheckOneDef ::
  (MonadError ModuleError m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier, DepType ResolvedDep Annotation) ->
  m (DepType ResolvedDep (Type ResolvedDep Annotation))
typecheckOneDef input inputModule deps (def, dep) =
  case dep of
    DTExpr expr ->
      DTExpr
        <$> typecheckOneExprDef
          input
          inputModule
          (filterExprs deps)
          (def, expr)
    DTData dt ->
      DTData
        <$> typecheckOneTypeDef
          input
          inputModule
          (filterDataTypes deps)
          (def, dt)

-- typechecking in this context means "does this data type make sense"
-- and "do we know about all external datatypes it mentions"
typecheckOneTypeDef ::
  (MonadError ModuleError m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (DataType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier, DataType ResolvedDep Annotation) ->
  m (DataType ResolvedDep (Type ResolvedDep Annotation))
typecheckOneTypeDef _input _inputModule _typeDeps (_def, _dt) = do
  pure undefined

{-
-- ideally we'd attach annotations to the DefIdentifiers or something, so we
-- can show the original code in errors
let ann = mempty

let action = do
      --validateConstructorsArentBuiltIns ann dt
      validateDataTypeVariables ann dt

-- typecheck it
liftEither $
  first
    ( DefDoesNotTypeCheck input def)
    action

pure dt
-}

-- given types for other required definition, typecheck a definition
typecheckOneExprDef ::
  (MonadError ModuleError m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (Expr ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier, Expr ResolvedDep Annotation) ->
  m (Expr ResolvedDep (Type ResolvedDep Annotation))
typecheckOneExprDef input _inputModule deps (def, expr) = do
  let _typeMap = getExprAnnotation <$> filterNameDefs deps

  -- initial typechecking environment
  -- env <- createTypecheckEnvironment inputModule deps typecheckedModules

  -- typecheck it
  liftEither $
    first
      (DefDoesNotTypeCheck input def)
      (elaborate expr)
