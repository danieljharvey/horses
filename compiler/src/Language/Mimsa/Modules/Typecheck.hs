{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Modules.Typecheck (typecheckAllModules) where

import Data.Foldable
import Language.Mimsa.TypeUtils
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import Language.Mimsa.Modules.Dependencies
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Typechecker.CreateEnv
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Utils

-- given the upstream modules, typecheck a module
-- 1. recursively fetch imports from Reader environment
-- 2. setup builder input
-- 3. do it!
typecheckAllModules ::
  Module Annotation ->
  CheckM (Map ModuleHash (Module (Type Annotation)))
typecheckAllModules rootModule = do
  modules <- asks ceModules
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <- getModuleDeps modules rootModule

  let stInputs =
        ( \(mod', deps) ->
            Build.Plan
              { Build.jbDeps = deps,
                Build.jbInput = mod'
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
    <$> Build.doJobs typecheckAllModuleDefs state

--- typecheck a single module
typecheckAllModuleDefs ::
  Map ModuleHash (Module (Type Annotation)) ->
  Module Annotation ->
  CheckM (Module (Type Annotation))
typecheckAllModuleDefs typecheckedDeps inputModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <- getValueDependencies inputModule
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
      <$> Build.doJobs (typecheckOneDef inputModule typecheckedDeps) state

  -- replace input module with typechecked versions
  pure $
    inputModule
      { moExpressions = filterExprs typecheckedDefs
      }

-- return type of module as a MTRecord of dep -> monotype
-- TODO: module should probably be it's own MTModule or something
-- as we'll want to pass them about at some point I think
getModuleType :: Module (Type Annotation) -> Type Annotation
getModuleType mod' =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in MTRecord mempty (getTypeFromAnn <$> filterNameDefs defs)

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
                      Just foundModule -> getModuleDataTypesByConstructor foundModule
                      _ -> mempty
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

filterNameDefs :: Map DefIdentifier a -> Map Name a
filterNameDefs =
  filterMapKeys
    ( \case
        DIName name -> Just name
        _ -> Nothing
    )

filterInfixDefs :: Map DefIdentifier a -> Map InfixOp a
filterInfixDefs =
  filterMapKeys
    ( \case
        DIInfix infixOp -> Just infixOp
        _ -> Nothing
    )

createTypecheckEnvironment ::
  Module Annotation ->
  Map DefIdentifier (Expr Name MonoType) ->
  Map ModuleHash (Module (Type Annotation)) ->
  CheckM Environment
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
      (getTypeFromAnn <$> filterInfixDefs (deps <> importedDeps))
      (getModuleTypes inputModule typecheckedModules)

getModuleTypes ::
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map ModuleHash (Map Name MonoType)
getModuleTypes inputModule typecheckedModules =
  let getTypes hash = case M.lookup hash typecheckedModules of
        Just mod' -> case getModuleType mod' of
          MTRecord _ parts -> (hash, parts)
          _ -> error "expected getModuleType to return a MTRecord but it did not"
        Nothing -> (hash, mempty)
   in M.fromList (getTypes <$> M.elems (moNamedImports inputModule))

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
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map DefIdentifier (DepType MonoType) ->
  (DefIdentifier, DepType Annotation) ->
  CheckM (DepType MonoType)
typecheckOneDef inputModule typecheckedModules deps (def, dep) =
  case dep of
    DTExpr expr ->
      DTExpr
        <$> typecheckOneExprDef inputModule typecheckedModules (filterExprs deps) (def, expr)
    DTData dt ->
      DTData
        <$> typecheckOneTypeDef inputModule typecheckedModules (filterDataTypes deps) (def, dt)

-- typechecking in this context means "does this data type make sense"
-- and "do we know about all external datatypes it mentions"
typecheckOneTypeDef ::
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map DefIdentifier DataType ->
  (DefIdentifier, DataType) ->
  CheckM DataType
typecheckOneTypeDef _inputModule _typecheckedModules typeDeps (def, dt) = do
  input <- getStoredInput
  
  let depsKeyedByTypeName = _ typeDeps

  -- ideally we'd attach annotations to the DefIdentifiers or something, so we
  -- can show the original code in errors
  let ann = mempty

  let action = do
                      validateConstructorsArentBuiltIns ann dt
                      validateDataTypeVariables ann dt
                      validateDataTypeUses depsKeyedByTypeName ann dt

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

-- type Broken a = Broken (Maybe a a)
-- should not make sense because it's using `Maybe` wrong
validateDataTypeUses :: (MonadError TypeError m) =>
  Map (Maybe ModuleName, TypeName)  DataType ->
  Annotation -> DataType -> m ()
validateDataTypeUses _deps _ann _dt = pure ()

-- which vars are used in this type?
getVariablesInType :: Type ann -> Set Name
getVariablesInType (MTVar _ (TVScopedVar _ name)) = S.singleton name
getVariablesInType (MTVar _ (TVName n)) = S.singleton (coerce n)
getVariablesInType other = withMonoid getVariablesInType other

-- given types for other required definition, typecheck a definition
typecheckOneExprDef ::
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map DefIdentifier (Expr Name MonoType) ->
  (DefIdentifier, Expr Name Annotation) ->
  CheckM (Expr Name MonoType)
typecheckOneExprDef inputModule typecheckedModules deps (def, expr) = do
  let typeMap = getTypeFromAnn <$> filterNameDefs deps
  input <- getStoredInput

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



