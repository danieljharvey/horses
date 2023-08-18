{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Modules.Typecheck (typecheckModule) where

import qualified Builder as Build
import Control.Monad.Except
import Control.Monad.Identity
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Smol.Core
import Smol.Core.Helpers
import Smol.Core.Modules.Dependencies
import Smol.Core.Modules.Helpers (filterNameDefs, filterTypeDefs)
import Smol.Core.Modules.Types
import Smol.Core.Modules.Types.DepType
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Typecheck.Typecheck (typecheck)
import Smol.Core.Typecheck.Typeclass (checkInstance, lookupTypeclass, resolveType, toIdentityExpr)
import Smol.Core.Typecheck.Typeclass.BuiltIns

-- go through the module, and wrap all the items in DefIdentifier keys and
-- DepType for items
getModuleDefIdentifiers ::
  Map DefIdentifier (Set DefIdentifier) ->
  Module dep ann ->
  Map DefIdentifier (DefIdentifier, DepType dep ann, Set DefIdentifier)
getModuleDefIdentifiers depMap inputModule =
  let getDeps di = fromMaybe mempty (M.lookup di depMap)
      exprs =
        M.fromList $
          ( \(name, expr) ->
              let defId = DIName name
               in (defId, (defId, DTExpr expr, getDeps (DIName name)))
          )
            <$> M.toList (moExpressions inputModule)
      dataTypes =
        M.fromList $
          ( \dt ->
              let defId = DIType (dtName dt)
               in (defId, (defId, DTData dt, getDeps defId))
          )
            <$> M.elems (moDataTypes inputModule)
      instances =
        M.fromList $
          ( \(constraint, inst) ->
              let defId = DIInstance constraint
               in (defId, (defId, DTInstance inst, getDeps defId))
          )
            <$> M.toList (moInstances inputModule)
   in exprs <> dataTypes <> instances

moduleFromDepTypes ::
  Module ResolvedDep ann ->
  Map DefIdentifier (DepType ResolvedDep (Type ResolvedDep ann)) ->
  Module ResolvedDep (Type ResolvedDep ann)
moduleFromDepTypes oldModule definitions =
  let firstMaybe f (a, b) = case f a of
        Just fa -> Just (fa, b)
        Nothing -> Nothing

      mapKeyMaybe f =
        M.fromList . mapMaybe (firstMaybe f) . M.toList

      getTypeName (DIType tn) = Just tn
      getTypeName _ = Nothing

      typedExpressions =
        M.fromList $
          mapMaybe
            ( \(k, a) -> case (k, a) of
                (DIName name, expr) -> Just (name, expr)
                _ -> Nothing
            )
            (M.toList $ filterExprs definitions)

      typedInstances =
        M.fromList $
          mapMaybe
            ( \(k, a) -> case (k, a) of
                (DIInstance constraint, DTInstance inst) -> Just (constraint, inst)
                _ -> Nothing
            )
            (M.toList definitions)

      typedClasses =
        (\tc -> tc $> resolveType (tcFuncType tc))
          <$> moClasses oldModule
   in -- replace input module with typechecked versions

      oldModule
        { moExpressions = typedExpressions,
          moDataTypes = mapKeyMaybe getTypeName (filterDataTypes definitions),
          moInstances = typedInstances,
          moClasses = typedClasses
        }

--- typecheck a single module
typecheckModule ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (Set DefIdentifier) ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
typecheckModule input inputModule depMap = do
  let inputWithDepsAndName = getModuleDefIdentifiers depMap inputModule

  let stInputs =
        ( \(name, expr, deps) ->
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
      <$> Build.doJobs (typecheckDef input inputModule) state

  -- check all tests make sense
  traverse_ (typecheckTest typecheckedDefs) (moTests inputModule)

  -- replace input module with typechecked versions
  pure $ moduleFromDepTypes inputModule typecheckedDefs

-- our "typecheck" is "get the expression, typecheck it against `Boolean`"
typecheckTest ::
  (MonadError (ModuleError Annotation) m) =>
  Map DefIdentifier (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  Test ->
  m ()
typecheckTest defs (UnitTest testName ident) = do
  case M.lookup (DIName ident) defs of
    Just (DTExpr tle) -> do
      let ty = getExprAnnotation (tleExpr tle)
      case ty of
        TPrim _ TPBool -> pure ()
        TLiteral _ (TLBool _) -> pure ()
        other ->
          throwError
            ( ErrorInTest
                testName
                ( TestDoesNotTypecheck
                    mempty
                    ident
                    (TCTypeMismatch other (TPrim (getTypeAnnotation other) TPBool))
                )
            )
    _ -> throwError (ErrorInResolveDeps $ VarNotFound ident)

-- given types for other required definition, typecheck a definition
typecheckDef ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier, DepType ResolvedDep Annotation) ->
  m (DepType ResolvedDep (Type ResolvedDep Annotation))
typecheckDef input inputModule deps (def, dep) =
  case dep of
    DTExpr expr ->
      DTExpr
        <$> typecheckExprDef
          input
          inputModule
          deps
          (def, expr)
    DTInstance inst ->
      DTInstance <$> typecheckInstance input inputModule deps def inst
    DTData dt ->
      DTData
        <$> typecheckTypeDef
          input
          inputModule
          (filterDataTypes deps)
          (def, dt)

typecheckInstance ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  DefIdentifier ->
  Instance Annotation ->
  m (Instance (Type ResolvedDep Annotation))
typecheckInstance input inputModule deps def inst = do
  -- where are we getting constraints from?
  let exprTypeMap =
        mapKey LocalDefinition $
          (\depTLE -> ((fmap . fmap) getTypeAnnotation (tleConstraints depTLE), getExprAnnotation (tleExpr depTLE)))
            <$> filterNameDefs (filterExprs deps)

  let constraint = case def of
        DIInstance c -> c
        _ -> error "def is not constraint, yikes"

  let instances :: Map (Constraint Annotation) (Instance Annotation)
      instances = mapKey (fmap (const mempty)) (moInstances inputModule)

      classes = moClasses inputModule

  -- initial typechecking environment
  let env =
        TCEnv
          { tceVars = exprTypeMap,
            tceDataTypes = getDataTypeMap deps,
            tceClasses = builtInClasses <> classes,
            tceInstances = builtInInstances <> instances,
            tceConstraints = mempty -- tleConstraints tle
          }

  typeclass <-
    modifyError
      (DefDoesNotTypeCheck input def)
      (lookupTypeclass env (conTypeclass constraint))

  (_fnName, constraints, typedExpr) <-
    modifyError (DefDoesNotTypeCheck input def) (checkInstance env typeclass (constraint $> mempty) inst)

  pure $
    Instance
      { inExpr = toIdentityExpr typedExpr,
        inConstraints = typeForConstraint <$> constraints
      }

-- typechecking in this context means "does this data type make sense"
-- and "do we know about all external datatypes it mentions"
typecheckTypeDef ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (DataType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier, DataType ResolvedDep Annotation) ->
  m (DataType ResolvedDep (Type ResolvedDep Annotation))
typecheckTypeDef _input _inputModule _typeDeps (_def, dt) = do
  -- just put a bullshit type in for now
  pure $ (`TPrim` TPBool) <$> dt

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

getDataTypeMap ::
  Map DefIdentifier (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  Map (ResolvedDep TypeName) (DataType ResolvedDep Annotation)
getDataTypeMap =
  (fmap . fmap) getTypeAnnotation
    . mapKey LocalDefinition
    . filterTypeDefs
    . filterDataTypes

resolveConstraint :: Constraint ann -> Constraint (Type ResolvedDep ann)
resolveConstraint (Constraint tcn tys) =
  Constraint tcn (resolveTy <$> tys)
  where
    resolveTy ty = ty $> toResolvedDep ty
    toResolvedDep = mapTypeDep (LocalDefinition . runIdentity)

typeForConstraint :: Constraint ann -> Constraint (Type ResolvedDep ann)
typeForConstraint (Constraint tc tys) =
  Constraint tc $ fmap (\ty -> ty $> resolveType ty) tys

-- given types for other required definition, typecheck a definition
typecheckExprDef ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier, TopLevelExpression ResolvedDep Annotation) ->
  m (TopLevelExpression ResolvedDep (Type ResolvedDep Annotation))
typecheckExprDef input inputModule deps (def, tle) = do
  -- where are we getting constraints from?
  let exprTypeMap =
        mapKey LocalDefinition $
          (\depTLE -> ((fmap . fmap) getTypeAnnotation (tleConstraints depTLE), getExprAnnotation (tleExpr depTLE)))
            <$> filterNameDefs (filterExprs deps)

  let instances :: Map (Constraint Annotation) (Instance Annotation)
      instances = mapKey (fmap (const mempty)) (moInstances inputModule)

      classes = moClasses inputModule

  -- initial typechecking environment
  let env =
        TCEnv
          { tceVars = exprTypeMap,
            tceDataTypes = getDataTypeMap deps,
            tceClasses = builtInClasses <> classes,
            tceInstances = builtInInstances <> instances,
            tceConstraints = tleConstraints tle
          }

  -- if we have a type, add an annotation
  let actualExpr = case tleType tle of
        Nothing -> tleExpr tle
        Just ty -> EAnn (getTypeAnnotation ty) ty (tleExpr tle)

  -- typecheck it
  (constraints, newExpr) <-
    liftEither $
      first
        (DefDoesNotTypeCheck input def)
        (typecheck env actualExpr)

  -- split the type out again
  let (typedType, typedExpr) = case newExpr of
        (EAnn _ ty expr) -> (Just ty, expr)
        other -> (Nothing, other)

  -- add supplied constraints to any we discovered in typechecking
  let allConstraints = nub (fmap resolveConstraint $ constraints <> tleConstraints tle)

  let typedTle =
        TopLevelExpression
          { tleConstraints = allConstraints,
            tleExpr = typedExpr,
            tleType = typedType
          }

  pure typedTle
