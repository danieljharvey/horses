{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Modules.Check (checkModule, exprAndTypeFromParts) where

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
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Prelude
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

-- this is where we keep all the modules we need to do things
newtype CheckEnv ann = CheckEnv
  { ceModules :: Map ModuleHash (Module ann)
  }

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

newtype CheckM a = CheckM
  { runCheckM ::
      ExceptT
        (Error Annotation)
        ( Reader (CheckEnv Annotation)
        )
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError (Error Annotation),
      MonadReader (CheckEnv Annotation)
    )

runCheck :: CheckM a -> Either (Error Annotation) a
runCheck comp = runReader (runExceptT (runCheckM comp)) initialEnv
  where
    initialEnv =
      CheckEnv
        { ceModules = M.singleton preludeHash prelude
        }

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

makeTypeDeclMap :: Module ann -> Map TyCon DataType
makeTypeDeclMap inputModule =
  M.fromList . fmap (first coerce) . M.toList $ moDataTypes inputModule

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

  pure $
    createEnv
      (getTypeFromAnn <$> (deps <> importedDeps))
      (makeTypeDeclMap inputModule)

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

moduleFromModuleParts ::
  (Monoid ann) =>
  [ModuleItem ann] ->
  CheckM (Module ann)
moduleFromModuleParts parts =
  let addPart part output = do
        mod' <- output
        case part of
          ModuleExport modItem -> do
            -- get whatever is inside
            innerModule <- addPart modItem output
            -- get the keys, add them to exports
            pure $
              innerModule
                { moExpressionExports =
                    M.keysSet (moExpressions innerModule)
                }
          ModuleExpression name bits expr ->
            case M.lookup name (moExpressions mod') of
              Just _ -> throwError (ModuleErr $ DuplicateDefinition name)
              Nothing ->
                let exp' = exprAndTypeFromParts bits expr
                 in pure $
                      mod'
                        { moExpressions =
                            M.singleton name exp' <> moExpressions mod'
                        }
          ModuleDataType dt@(DataType tyCon _ _) ->
            let typeName = coerce tyCon
             in case M.lookup typeName (moDataTypes mod') of
                  Just _ -> throwError (ModuleErr $ DuplicateTypeName typeName)
                  Nothing ->
                    pure $
                      mod'
                        { moDataTypes =
                            M.singleton typeName dt
                              <> moDataTypes mod'
                        }
          ModuleImport (ImportAllFromHash mHash) -> do
            importMod <- lookupModule mHash

            let createImports =
                  M.fromList
                    . fmap (,mHash)
                    . S.toList
                    . moExpressionExports
            pure $
              mod'
                { moExpressionImports =
                    createImports importMod
                      <> moExpressionImports mod'
                }
   in foldr addPart (pure mempty) parts

addAnnotation :: Maybe (Type ann) -> Expr Name ann -> Expr Name ann
addAnnotation mt expr =
  -- add type annotation to expression
  case mt of
    Just typeAnnotation ->
      MyAnnotation
        (getAnnotationForType typeAnnotation)
        typeAnnotation
        expr
    _ -> expr

-- given the bits of things, make a coherent type and expression
exprAndTypeFromParts ::
  (Monoid ann) =>
  [DefPart ann] ->
  Expr Name ann ->
  Expr Name ann
exprAndTypeFromParts parts expr =
  let expr' =
        foldr
          ( \part rest -> case part of
              (DefArg ident) -> MyLambda mempty ident rest
              (DefTypedArg ident _) -> MyLambda mempty ident rest
              (DefType _) -> rest
          )
          expr
          parts
      -- if we only have un-typed args, don't bother, we only want them as
      -- placeholders
      filteredParts =
        let includesExplicitTypes =
              any
                ( \case
                    (DefArg _) -> False
                    _ -> True
                )
                parts
            includesReturnType =
              any
                ( \case
                    (DefType _) -> True
                    _ -> False
                )
                parts
         in if includesExplicitTypes
              then
                if includesReturnType
                  then parts
                  else parts <> [DefType (MTVar mempty (TVName "returnType"))]
              else mempty
      mt =
        foldr
          ( \part rest -> case part of
              (DefArg (Identifier _ name)) -> case rest of
                Just rest' ->
                  Just
                    ( MTFunction
                        mempty
                        (MTVar mempty (TVName (coerce name)))
                        rest'
                    )
                Nothing ->
                  Just
                    ( MTVar
                        mempty
                        (TVName (coerce name))
                    )
              (DefTypedArg _ thisMt) -> case rest of
                Just rest' ->
                  Just
                    (MTFunction mempty thisMt rest')
                _ -> Just thisMt
              (DefType thisMt) -> case rest of
                Just rest' ->
                  Just
                    (MTFunction mempty rest' thisMt)
                _ -> Just thisMt
          )
          Nothing
          filteredParts
   in addAnnotation mt expr'
