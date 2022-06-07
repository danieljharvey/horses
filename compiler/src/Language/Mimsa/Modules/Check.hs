{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Modules.Check (checkModule, typecheckAllModules, lookupModuleDefType) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import Language.Mimsa.Modules.Dependencies
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Parse
import Language.Mimsa.Typechecker.CreateEnv
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker

checkModule ::
  Text ->
  Map ModuleHash (Module Annotation) ->
  Either (Error Annotation) (Module (Type Annotation), MonoType)
checkModule input modules = runCheck input modules (checkModule' input)

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
checkModule' :: Text -> CheckM (Module (Type Annotation), MonoType)
checkModule' input = do
  properMod <- parseModule' input

  -- typecheck this module
  tcMods <- typecheckAllModules properMod

  let rootModuleHash = hashModule properMod

  case M.lookup rootModuleHash tcMods of
    Nothing -> throwError (ModuleErr $ MissingModule rootModuleHash)
    Just tcMod -> pure (tcMod, getModuleType tcMod)

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

  let state =
        Build.State
          { Build.stInputs =
              ( \(mod', deps) ->
                  Build.Plan
                    { Build.jbDeps = deps,
                      Build.jbInput = mod'
                    }
              )
                <$> inputWithDeps,
            Build.stOutputs = mempty
          }
  -- go!
  Build.stOutputs
    <$> Build.doJobs typecheckAllModuleDefs state

--- typecheck a module
typecheckAllModuleDefs ::
  Map ModuleHash (Module (Type Annotation)) ->
  Module Annotation ->
  CheckM (Module (Type Annotation))
typecheckAllModuleDefs typecheckedDeps inputModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <- getValueDependencies inputModule
  let inputWithDepsAndName = M.mapWithKey (,) inputWithDeps

  let state =
        Build.State
          { Build.stInputs =
              ( \(name, (expr, deps, _)) ->
                  Build.Plan
                    { Build.jbDeps = deps,
                      Build.jbInput = (name, expr)
                    }
              )
                <$> inputWithDepsAndName,
            Build.stOutputs = mempty
          }
  -- go!
  typecheckedDefs <-
    Build.stOutputs
      <$> Build.doJobs (typecheckOneDef inputModule typecheckedDeps) state

  -- replace input module with typechecked versions
  pure $
    inputModule
      { moExpressions = typecheckedDefs
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

lookupModuleDefType :: Module (Type Annotation) -> DefIdentifier -> Maybe (Type Annotation)
lookupModuleDefType mod' defId =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in getTypeFromAnn <$> M.lookup defId defs

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
-- really the key is TypeName, but we need to untangle that still
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

-- mess with map keys for lols
mapKeys :: (Ord k2) => (k -> k2) -> Map k a -> Map k2 a
mapKeys f = M.fromList . fmap (first f) . M.toList

-- useful to break apart maps where
-- key is a sum type
filterMapKeys :: (Ord k2) => (k -> Maybe k2) -> Map k a -> Map k2 a
filterMapKeys f =
  M.fromList . mapMaybe (\(k, a) -> (,) <$> f k <*> pure a) . M.toList

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
  Map DefIdentifier (Expr Name MonoType) ->
  (DefIdentifier, Expr Name Annotation) ->
  CheckM (Expr Name MonoType)
typecheckOneDef inputModule typecheckedModules deps (def, expr) = do
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
