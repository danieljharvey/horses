{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Smol.Modules.Typecheck (typecheckModule) where

import qualified Builder as Build
import Control.Monad.Except
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
import Smol.Modules.Dependencies
import Smol.Modules.Helpers (filterNameDefs, filterTypeDefs)
import Smol.Modules.Types
import Smol.Modules.Types.DepType
import Smol.Modules.Types.ModuleError
import Smol.Core.Typecheck.Typecheck (typecheck)
import Smol.Core.Typecheck.Typeclass (addTypesToConstraint, checkInstance, lookupTypeclass)

-- go through the module, and wrap all the items in DefIdentifier keys and
-- DepType for items
getModuleDefIdentifiers ::
  (Ord (dep Constructor), Ord (dep TypeName), Ord (dep Identifier)) =>
  Map (DefIdentifier dep) (Set (DefIdentifier dep)) ->
  Module dep ann ->
  Map (DefIdentifier dep) (DefIdentifier dep, DepType dep ann, Set (DefIdentifier dep))
getModuleDefIdentifiers depMap inputModule =
  let getDeps di = fromMaybe mempty (M.lookup di depMap)
      exprs =
        M.fromList $
          ( \(name, expr) ->
              let defId = DIName name
               in (defId, (defId, DTExpr expr, getDeps defId))
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
      tests =
        M.fromList $
          ( \(UnitTest testName expr) ->
              let defId = DITest testName
               in (defId, (defId, DTTest expr, getDeps defId))
          )
            <$> moTests inputModule
   in exprs <> dataTypes <> instances <> tests

moduleFromDepTypes ::
  Module ResolvedDep ann ->
  Map (DefIdentifier ResolvedDep) (DepType ResolvedDep (Type ResolvedDep ann)) ->
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
            ( \case
                (DIName name, expr) -> Just (name, expr)
                _ -> Nothing
            )
            (M.toList $ filterExprs definitions)

      typedInstances =
        M.fromList $
          mapMaybe
            ( \case
                (DIInstance constraint, DTInstance inst) -> Just (constraint, inst)
                _ -> Nothing
            )
            (M.toList definitions)

      typedClasses =
        (\tc -> tc $> tcFuncType tc)
          <$> moClasses oldModule

      typedTests =
        mapMaybe
          ( \case
              (DITest testName, DTTest expr) -> Just (UnitTest testName expr)
              _ -> Nothing
          )
          (M.toList definitions)
   in -- replace input module with typechecked versions

      oldModule
        { moExpressions = typedExpressions,
          moDataTypes = mapKeyMaybe getTypeName (filterDataTypes definitions),
          moInstances = typedInstances,
          moClasses = typedClasses,
          moTests = typedTests
        }

--- typecheck a single module
typecheckModule ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map (DefIdentifier ResolvedDep) (Set (DefIdentifier ResolvedDep)) ->
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

  -- replace input module with typechecked versions
  let newModule = moduleFromDepTypes inputModule typecheckedDefs

  -- check tests are the right types
  traverse_ (ensureTestsAreBooleans typecheckedDefs) (moTests newModule)

  pure newModule

-- our "typecheck" is "get the expression, typecheck it against `Boolean`"
ensureTestsAreBooleans ::
  (MonadError (ModuleError Annotation) m) =>
  Map (DefIdentifier ResolvedDep) (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  Test ResolvedDep (Type ResolvedDep Annotation) ->
  m ()
ensureTestsAreBooleans _defs (UnitTest testName expr) = do
  let ty = getExprAnnotation expr
  case ty of
    TPrim _ TPBool -> pure ()
    TLiteral _ (TLBool _) -> pure ()
    other ->
      throwError
        ( ErrorInTest
            testName
            ( TestDoesNotTypecheck
                mempty
                (TCTypeMismatch other (TPrim (getTypeAnnotation other) TPBool))
            )
        )

-- given types for other required definition, typecheck a definition
typecheckDef ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map (DefIdentifier ResolvedDep) (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier ResolvedDep, DepType ResolvedDep Annotation) ->
  m (DepType ResolvedDep (Type ResolvedDep Annotation))
typecheckDef input inputModule deps (def, dep) =
  case dep of
    DTExpr expr ->
      DTExpr
        <$> typecheckExprDef
          inputModule
          deps
          (def, expr)
    DTInstance inst ->
      DTInstance <$> typecheckInstance inputModule deps def inst
    DTTest expr ->
      DTTest . tleExpr <$> typecheckExprDef inputModule deps (def, TopLevelExpression {tleConstraints = mempty, tleExpr = expr, tleType = Nothing})
    DTData dt ->
      DTData
        <$> typecheckTypeDef
          input
          inputModule
          (filterDataTypes deps)
          (def, dt)

typecheckInstance ::
  (MonadError (ModuleError Annotation) m) =>
  Module ResolvedDep Annotation ->
  Map (DefIdentifier ResolvedDep) (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  DefIdentifier ResolvedDep ->
  Instance ResolvedDep Annotation ->
  m (Instance ResolvedDep (Type ResolvedDep Annotation))
typecheckInstance inputModule deps def inst = do
  -- where are we getting constraints from?
  let exprTypeMap =
        mapKey LocalDefinition $
          (\depTLE -> ((fmap . fmap) getTypeAnnotation (tleConstraints depTLE), getExprAnnotation (tleExpr depTLE)))
            <$> filterNameDefs (filterExprs deps)

  let constraint = case def of
        DIInstance c -> c
        _ -> error "def is not constraint, yikes"

  let instances :: Map (Constraint ResolvedDep Annotation) (Instance ResolvedDep Annotation)
      instances = mapKey (fmap (const mempty)) (moInstances inputModule)

      classes = moClasses inputModule

  -- initial typechecking environment
  let env =
        TCEnv
          { tceVars = exprTypeMap,
            tceDataTypes = getDataTypeMap deps,
            tceClasses = classes,
            tceInstances = instances,
            tceConstraints = inConstraints inst
          }

  typeclass <-
    modifyError
      (DefDoesNotTypeCheck def)
      (lookupTypeclass classes (conTypeclass constraint))

  let typedConstraint = addTypesToConstraint (constraint $> mempty)

  modifyError (DefDoesNotTypeCheck def) (checkInstance env typeclass typedConstraint inst)

-- typechecking in this context means "does this data type make sense"
-- and "do we know about all external datatypes it mentions"
typecheckTypeDef ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map (DefIdentifier ResolvedDep) (DataType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier ResolvedDep, DataType ResolvedDep Annotation) ->
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
  Map (DefIdentifier ResolvedDep) (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  Map (ResolvedDep TypeName) (DataType ResolvedDep Annotation)
getDataTypeMap =
  (fmap . fmap) getTypeAnnotation
    . mapKey LocalDefinition
    . filterTypeDefs
    . filterDataTypes

resolveConstraint :: Constraint ResolvedDep ann -> Constraint ResolvedDep (Type ResolvedDep ann)
resolveConstraint (Constraint tcn tys) =
  Constraint tcn (resolveTy <$> tys)
  where
    resolveTy ty = ty $> ty

-- given types for other required definition, typecheck a definition
typecheckExprDef ::
  (MonadError (ModuleError Annotation) m) =>
  Module ResolvedDep Annotation ->
  Map (DefIdentifier ResolvedDep) (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier ResolvedDep, TopLevelExpression ResolvedDep Annotation) ->
  m (TopLevelExpression ResolvedDep (Type ResolvedDep Annotation))
typecheckExprDef inputModule deps (def, tle) = do
  -- where are we getting constraints from?
  let exprTypeMap =
        mapKey LocalDefinition $
          (\depTLE -> ((fmap . fmap) getTypeAnnotation (tleConstraints depTLE), getExprAnnotation (tleExpr depTLE)))
            <$> filterNameDefs (filterExprs deps)

  let instances :: Map (Constraint ResolvedDep Annotation) (Instance ResolvedDep Annotation)
      instances = mapKey (fmap (const mempty)) (moInstances inputModule)

      classes = moClasses inputModule

  -- initial typechecking environment
  let env =
        TCEnv
          { tceVars = exprTypeMap,
            tceDataTypes = getDataTypeMap deps,
            tceClasses = classes,
            tceInstances = instances,
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
        (DefDoesNotTypeCheck def)
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
