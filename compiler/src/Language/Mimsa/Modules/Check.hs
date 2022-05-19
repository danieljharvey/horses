{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Modules.Check (checkModule) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import Language.Mimsa.Modules.FromParts
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Parser.Module
import Language.Mimsa.Store
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker

lookupModule :: ModuleHash -> CheckM (Module Annotation)
lookupModule modHash = do
  mods <- asks ceModules
  case M.lookup modHash mods of
    Just foundModule -> pure foundModule
    _ -> throwError (ModuleErr (MissingModule modHash))

lookupModuleDep ::
  Map ModuleHash (Module (Type Annotation)) ->
  Name ->
  ModuleHash ->
  CheckM (Expr Name (Type Annotation))
lookupModuleDep typecheckedModules name modHash = do
  case M.lookup modHash typecheckedModules of
    Just mod' ->
      case M.lookup name (moExpressions mod') of
        Just expr -> pure expr
        _ -> throwError (ModuleErr (MissingModuleDep name modHash))
    _ -> throwError (ModuleErr (MissingModule modHash))

lookupModuleType ::
  Map ModuleHash (Module (Type Annotation)) ->
  TypeName ->
  ModuleHash ->
  CheckM DataType
lookupModuleType typecheckedModules typeName modHash = do
  case M.lookup modHash typecheckedModules of
    Just mod' ->
      case M.lookup typeName (moDataTypes mod') of
        Just dt -> pure dt
        _ -> throwError (ModuleErr (MissingModuleTypeDep typeName modHash))
    _ -> throwError (ModuleErr (MissingModule modHash))

checkModule :: Text -> Either (Error Annotation) (Module (Type Annotation), MonoType)
checkModule = runCheck . checkModule'

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
--
--  soon there will also need to be
--  1. infix definitions
--  2. tests
--  3. property tests
--  4. metadata / comments etc?
checkModule' :: Text -> CheckM (Module (Type Annotation), MonoType)
checkModule' input = do
  moduleItems <-
    liftEither $
      first (ParseError input) (parseModule input)
  -- create module from parsed items
  properMod <-
    moduleFromModuleParts moduleItems
  -- typecheck this module
  tcMod <- typecheckAllModules properMod

  pure (tcMod, getModuleType tcMod)

-- return type of module as a MTRecord of dep -> monotype
-- TODO: module should probably be it's own MTModule or something
-- as we'll want to pass them about at some point I think
getModuleType :: Module (Type Annotation) -> Type Annotation
getModuleType mod' =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in MTRecord mempty (getTypeFromAnn <$> defs)

-- get the vars used by each def
-- explode if there's not available
getValueDependencies ::
  (Eq ann, Monoid ann) =>
  Module ann ->
  CheckM
    ( Map
        Name
        ( Expr Name ann,
          Set Name
        )
    )
getValueDependencies mod' = do
  let check exp' =
        let deps = extractVars exp'
            unknownDeps =
              S.filter
                ( \dep ->
                    S.notMember dep (M.keysSet (moExpressions mod'))
                      && S.notMember dep (M.keysSet (moExpressionImports mod'))
                )
                deps
         in if S.null unknownDeps
              then
                let localDeps =
                      S.filter
                        ( `S.member`
                            M.keysSet (moExpressions mod')
                        )
                        deps
                 in pure (exp', localDeps)
              else throwError (ModuleErr (CannotFindValues unknownDeps))
  traverse check (moExpressions mod')

-- typecheck a module
typecheckAllModuleDeps ::
  Map ModuleHash (Module (Type Annotation)) ->
  Module Annotation ->
  CheckM (Module (Type Annotation))
typecheckAllModuleDeps typecheckedDeps inputModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <- getValueDependencies inputModule
  let inputWithDepsAndName = M.mapWithKey (,) inputWithDeps

  let state =
        Build.State
          { Build.stInputs =
              ( \(name, (expr, deps)) ->
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
      <$> Build.doJobs (typecheckOneDep inputModule typecheckedDeps) state
  pure $ inputModule {moExpressions = typecheckedDefs}

makeTypeDeclMap :: Map TypeName DataType -> Module ann -> Map TyCon DataType
makeTypeDeclMap importedTypes inputModule =
  M.fromList . fmap (first coerce) . M.toList $
    moDataTypes inputModule
      <> importedTypes

createTypecheckEnvironment ::
  Module Annotation ->
  Map Name (Expr Name MonoType) ->
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
      (getTypeFromAnn <$> (deps <> importedDeps))
      (makeTypeDeclMap importedTypes inputModule)

-- starting at a root module,
-- create a map of each expr hash along with the modules it needs
-- so that we can typecheck them all
getModuleDeps :: Module Annotation -> CheckM (Map ModuleHash (Module Annotation, Set ModuleHash))
getModuleDeps inputModule = do
  -- get this module's deps
  let deps = S.fromList $ M.elems (moExpressionImports inputModule)
      mHash = hashModule inputModule
  -- recursively fetch sub-deps
  depModules <- traverse lookupModule (S.toList deps)
  subDeps <- traverse getModuleDeps depModules

  pure $ M.singleton mHash (inputModule, deps) <> mconcat subDeps

-- given up stream modules, typecheck a module
-- 1. recursively fetch imports from Reader environment
-- 2. setup builder input
-- 3. do it!
typecheckAllModules ::
  Module Annotation ->
  CheckM (Module (Type Annotation))
typecheckAllModules rootModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <- getModuleDeps rootModule

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
  allCheckedModules <-
    Build.stOutputs
      <$> Build.doJobs typecheckAllModuleDeps state

  -- TODO: cache it or something?
  -- lookup the original one
  -- return it
  case M.lookup (hashModule rootModule) allCheckedModules of
    Just mod' -> pure mod'
    _ -> error "could not find typechecked module"

-- given types for other required definition, typecheck a definition
typecheckOneDep ::
  Module Annotation ->
  Map ModuleHash (Module (Type Annotation)) ->
  Map Name (Expr Name MonoType) ->
  (Name, Expr Name Annotation) ->
  CheckM (Expr Name MonoType)
typecheckOneDep inputModule typecheckedModules deps (name, expr) = do
  let typeMap = getTypeFromAnn <$> deps
  -- number the vars
  numberedExpr <-
    liftEither $
      first
        (ModuleErr . DefDoesNotTypeCheck name)
        ( addNumbersToExpression
            (M.keysSet deps)
            (coerce <$> moExpressionImports inputModule)
            expr
        )
  -- initial typechecking environment
  env <- createTypecheckEnvironment inputModule deps typecheckedModules
  -- typecheck it
  (_subs, _constraints, typedExpr, _mt) <-
    liftEither $
      first
        (ModuleErr . DefDoesNotTypeCheck name)
        (typecheck typeMap env numberedExpr)
  pure (first fst typedExpr)
