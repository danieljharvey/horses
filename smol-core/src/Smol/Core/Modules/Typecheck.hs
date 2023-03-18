{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Smol.Core.Modules.Typecheck (typecheckAllModules) where

-- import Smol.Core.Types.Module.ModuleName
-- import Smol.Core.Types.Module.DefIdentifier

import Control.Monad.Except
-- import Data.Bifunctor
-- import Data.Coerce
-- import Data.Foldable
import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
-- import Data.Maybe
-- import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Text (Text)
-- import qualified Builder as Build
import Smol.Core
-- import Smol.Core.Modules.Dependencies
-- import Smol.Core.Modules.HashModule
-- import Smol.Core.Modules.Monad
-- import Smol.Core.Modules.Uses
import Smol.Core.Modules.ModuleError
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleHash

-- given the upstream modules, typecheck a module
-- 1. recursively fetch imports from Reader environment
-- 2. setup builder input
-- 3. do it!
typecheckAllModules ::
  (MonadError ModuleError m) =>
  Map ModuleHash (Module Annotation) ->
  Text ->
  Module Annotation ->
  m (Map ModuleHash (Module (Type ParseDep Annotation)))
typecheckAllModules _modules _rootModuleInput _rootModule = do
  pure mempty

{-
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <- getModuleDeps modules rootModule

  let (_, rootModuleHash) = serializeModule rootModule

  let stInputs =
        ( \(mod', deps) ->
            Build.Plan
              { Build.jbDeps = deps,
                Build.jbInput =
                  let (_, moduleHash) = serializeModule mod'
                   in if moduleHash == rootModuleHash
                        then (rootModuleInput, mod')
                        else (prettyPrint mod', mod')
              }
        )
          <$> inputWithDeps

  let state =
        Build.State
          { Build.stInputs = stInputs,
            Build.stOutputs = mempty
          }
  -- go!
  Build.stOutputs
    <$> Build.doJobsPure
      ( \deps (input, mod') ->
          typecheckAllModuleDefs deps input mod'
      )
      state

-}

{-
--- typecheck a single module
typecheckAllModuleDefs ::
  (MonadError ModuleError m) =>
  Map ModuleHash (Module (Type Annotation)) ->
  Text ->
  Module Annotation ->
  m (Module (Type Annotation))
typecheckAllModuleDefs typecheckedDeps input inputModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <- getDependencies extractUses inputModule
  let inputWithDepsAndName = M.mapWithKey (,) inputWithDeps

  let stInputs =
        ( \(name, (expr, deps, _)) ->
            Build.Plan
              { Build.jbDeps = deps,
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
      <$> Build.doJobsPure (typecheckOneDef input inputModule typecheckedDeps) state

  -- replace input module with typechecked versions
  pure $
    inputModule
      { moExpressions = filterExprs typecheckedDefs
      }

-- return type of module as a MTRecord of dep -> monotype
-- TODO: module should probably be it's own MTModule or something
-- as we'll want to pass them about at some point I think
-- also if any types defined in this module are used in it's types, namespace
-- those too
-- so if the module defines `export type Tree a = ...`
-- and is imported as `Tree`, change "Tree" types to "Tree.Tree" so they unify
getModuleType :: ModuleName -> Module (Type Annotation) -> Type Annotation
getModuleType modName mod' =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
      moduleTypeNames = M.keysSet (moDataTypes mod')
   in MTRecord
        mempty
        ( addNamespaceToType modName moduleTypeNames
            . getTypeFromAnn
            <$> filterNameDefs defs
        )
        Nothing

addNamespaceToType :: ModuleName -> Set TypeName -> Type Annotation -> Type Annotation
addNamespaceToType modName swapTypes =
  addNS
  where
    addNS old@(TConstructor ann Nothing typeName) =
      if S.member typeName swapTypes
        then TConstructor ann (Just modName) typeName
        else old
    addNS other = mapType addNS other

-- pass types to the typechecker
makeTypeDeclMap ::
  Map ModuleHash (Module (Type Annotation)) ->
  Map TypeName DataType ->
  Module ann ->
  Map (Maybe ModuleName, TypeName) DataType
makeTypeDeclMap typecheckedModules importedTypes inputModule =
  let regularScopeTypes =
        mapKeys
          (\tyCon -> (Nothing, coerce tyCon))
          ( moDataTypes inputModule
              <> importedTypes
          )
      namespacedTypes =
        mconcat $
          fmap
            ( \(modName, modHash) ->
                let byTyCon = case M.lookup modHash typecheckedModules of
                      Just foundModule ->
                        getModuleDataTypesByConstructor foundModule
                      _ -> error "could not find module"
                 in mapKeys (Just modName,) byTyCon
            )
            (M.toList $ moNamedImports inputModule)
   in regularScopeTypes <> namespacedTypes

-- for any given module, return a Map of its DataTypes
getModuleDataTypesByConstructor :: Module ann -> Map TypeName DataType
getModuleDataTypesByConstructor inputModule =
  let exportedDts =
        M.filterWithKey
          ( \k _ ->
              S.member
                k
                (moDataTypeExports inputModule)
          )
          (moDataTypes inputModule)
   in mapKeys coerce exportedDts

filterNameDefs :: Map DefIdentifier a -> Map Identifier a
filterNameDefs =
  filterMapKeys
    ( \case
        DIName name -> Just name
        _ -> Nothing
    )

createTypecheckEnvironment ::
  (MonadError ModuleError m) =>
  Module Annotation ->
  Map DefIdentifier (Expr Name MonoType) ->
  Map ModuleHash (Module (Type Annotation)) ->
  m Environment
createTypecheckEnvironment inputModule deps typecheckedModules = do
  -- these need to be typechecked
  importedDeps <-
    M.traverseWithKey
      (lookupModuleDep typecheckedModules)
      (moExpressionImports inputModule)

  importedTypes <-
    M.traverseWithKey
      (lookupModuleType typecheckedModules)
      (moDataTypeImports inputModule)

  pure $
    createEnv
      (getTypeFromAnn <$> filterNameDefs (deps <> importedDeps))
      (makeTypeDeclMap typecheckedModules importedTypes inputModule)
      (getModuleTypes inputModule typecheckedModules)

getModuleTypes ::
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map ModuleHash (Map Name MonoType)
getModuleTypes inputModule typecheckedModules =
  let getTypes (modName, hash) = case M.lookup hash typecheckedModules of
        Just mod' -> case getModuleType modName mod' of
          MTRecord _ parts _ -> (hash, parts)
          _ -> error "expected getModuleType to return a MTRecord but it did not"
        Nothing -> error "Could not find module for hash in getModuleTypes"
   in M.fromList (getTypes <$> M.toList (moNamedImports inputModule))

namespacedModules ::
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map ModuleName (ModuleHash, Set Name)
namespacedModules inputModule typecheckedModules =
  let getPair hash = case M.lookup hash typecheckedModules of
        Just mod' -> (hash, namesOnly (moExpressionExports mod'))
        Nothing -> (hash, mempty)
   in getPair <$> moNamedImports inputModule

namesOnly :: Set DefIdentifier -> Set Name
namesOnly =
  S.fromList
    . mapMaybe
      ( \case
          DIName name -> Just name
          _ -> Nothing
      )
    . S.toList

-- given types for other required definition, typecheck a definition
typecheckOneDef ::
  (MonadError ModuleError m) =>
  Text ->
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map DefIdentifier (DepType MonoType) ->
  (DefIdentifier, DepType Annotation) ->
  m (DepType MonoType)
typecheckOneDef input inputModule typecheckedModules deps (def, dep) =
  case dep of
    DTExpr expr ->
      DTExpr
        <$> typecheckOneExprDef input inputModule typecheckedModules (filterExprs deps) (def, expr)
    DTData dt ->
      DTData
        <$> typecheckOneTypeDef input inputModule typecheckedModules (filterDataTypes deps) (def, dt)

_keyDeps ::
  Module Annotation ->
  Map DefIdentifier DataType ->
  Map (Maybe ModuleName, TypeName) DataType
_keyDeps _mod =
  filterMapKeys
    ( \case
        DIType typeName -> Just (Nothing, typeName)
        _ -> Nothing
    )

-- typechecking in this context means "does this data type make sense"
-- and "do we know about all external datatypes it mentions"
typecheckOneTypeDef ::
  (MonadError ModuleError m) =>
  Text ->
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map DefIdentifier DataType ->
  (DefIdentifier, DataType) ->
  m DataType
typecheckOneTypeDef input _inputModule _typecheckedModules _typeDeps (def, dt) = do
  -- ideally we'd attach annotations to the DefIdentifiers or something, so we
  -- can show the original code in errors
  let ann = mempty

  let action = do
        validateConstructorsArentBuiltIns ann dt
        validateDataTypeVariables ann dt

  -- typecheck it
  liftEither $
    first
      (ModuleErr . DefDoesNotTypeCheck input def)
      action

  pure dt

-- when adding a new datatype, check none of the constructors are built in ones
validateConstructorsArentBuiltIns ::
  (MonadError TypeError m) =>
  Annotation ->
  DataType ->
  m ()
validateConstructorsArentBuiltIns ann (DataType _ _ constructors) = do
  traverse_
    ( \(tyCon, _) ->
        case lookupBuiltIn (coerce tyCon) of
          Just _ -> throwError (CannotUseBuiltInTypeAsConstructor ann (coerce tyCon))
          _ -> pure ()
    )
    (M.toList constructors)

-- check all types vars are part of dataytpe definition
-- `type Maybe a | Just b` would be a problem because `b` is a mystery
validateDataTypeVariables ::
  (MonadError TypeError m) =>
  Annotation ->
  DataType ->
  m ()
validateDataTypeVariables ann (DataType typeName vars constructors) =
  let requiredForCons = foldMap getVariablesInType
      requiredVars = foldMap requiredForCons constructors
      availableVars = S.fromList vars
      unavailableVars = S.filter (`S.notMember` availableVars) requiredVars
   in if S.null unavailableVars
        then pure ()
        else
          throwError $
            TypeVariablesNotInDataType ann typeName unavailableVars availableVars

-- which vars are used in this type?
getVariablesInType :: Type ann -> Set Name
getVariablesInType (MTVar _ (TVScopedVar _ name)) = S.singleton name
getVariablesInType (MTVar _ (TVName n)) = S.singleton (coerce n)
getVariablesInType other = withMonoidType getVariablesInType other

-- TODO: this is wrong
getConstructorUses :: Type ann -> Set (Maybe ModuleName, TypeName, Int)
getConstructorUses (MTConstructor _ modName typeName) =
  S.singleton (modName, typeName, 0)
getConstructorUses other = withMonoidType getConstructorUses other

-- given types for other required definition, typecheck a definition
typecheckOneExprDef ::
  (MonadError ModuleError m) =>
  Text ->
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map DefIdentifier (Expr Name MonoType) ->
  (DefIdentifier, Expr Name Annotation) ->
  m (Expr Name MonoType)
typecheckOneExprDef input inputModule typecheckedModules deps (def, expr) = do
  let typeMap = getTypeFromAnn <$> filterNameDefs deps

  -- number the vars
  numberedExpr <-
    liftEither $
      first
        (ModuleErr . DefDoesNotTypeCheck input def)
        ( addNumbersToExpression
            (M.keysSet (filterNameDefs deps))
            (coerce <$> filterNameDefs (moExpressionImports inputModule))
            (namespacedModules inputModule typecheckedModules)
            expr
        )

  -- initial typechecking environment
  env <- createTypecheckEnvironment inputModule deps typecheckedModules

  -- typecheck it
  (_subs, _constraints, typedExpr, _mt) <-
    liftEither $
      first
        (ModuleErr . DefDoesNotTypeCheck input def)
        (typecheck typeMap env numberedExpr)

  pure (first fst typedExpr)

-}
