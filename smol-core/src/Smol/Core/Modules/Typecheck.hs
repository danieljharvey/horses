{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Modules.Typecheck (typecheckModule) where

import Control.Monad.Identity
import qualified Builder as Build
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Smol.Core
import Smol.Core.Helpers (mapKey)
import Smol.Core.Modules.Dependencies
import Smol.Core.Modules.Helpers (filterNameDefs, filterTypeDefs)
import Smol.Core.Modules.Types
import Smol.Core.Modules.Types.DepType
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Typecheck.Typecheck (typecheck)

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
   in exprs <> dataTypes

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
      newExpressions =
        M.fromList $
          mapMaybe
            ( \(k, a) -> case (k, a) of
                (DIName name, expr) -> Just (name, expr)
                _ -> Nothing
            )
            (M.toList $ filterExprs definitions)
   in -- replace input module with typechecked versions
      oldModule
        { moExpressions = newExpressions,
          moDataTypes = mapKeyMaybe getTypeName (filterDataTypes definitions)
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
      <$> Build.doJobs (typecheckOneDef input inputModule) state

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
typecheckOneDef ::
  (MonadError (ModuleError Annotation) m) =>
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
          deps
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
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (DataType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier, DataType ResolvedDep Annotation) ->
  m (DataType ResolvedDep (Type ResolvedDep Annotation))
typecheckOneTypeDef _input _inputModule _typeDeps (_def, dt) = do
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
resolveConstraint (Constraint tcn tys)
  = Constraint tcn (resolveTy  <$> tys)
    where
      resolveTy ty = ty $> toResolvedDep ty
      toResolvedDep = mapTypeDep (LocalDefinition . runIdentity)

-- given types for other required definition, typecheck a definition
typecheckOneExprDef ::
  (MonadError (ModuleError Annotation) m) =>
  Text ->
  Module ResolvedDep Annotation ->
  Map DefIdentifier (DepType ResolvedDep (Type ResolvedDep Annotation)) ->
  (DefIdentifier, TopLevelExpression ResolvedDep Annotation) ->
  m (TopLevelExpression ResolvedDep (Type ResolvedDep Annotation))
typecheckOneExprDef input _inputModule deps (def, tle) = do
  -- where are we getting constraints from?
  let exprTypeMap =
        mapKey LocalDefinition $
          (\depTLE -> ((fmap . fmap) getTypeAnnotation (tleConstraints depTLE), getExprAnnotation (tleExpr depTLE)))
            <$> filterNameDefs (filterExprs deps)

  -- initial typechecking environment
  let env =
        TCEnv
          { tceVars = exprTypeMap,
            tceDataTypes = getDataTypeMap deps,
            tceClasses = mempty,
            tceInstances = mempty,
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

  pure
    ( TopLevelExpression
        { tleConstraints = fmap resolveConstraint constraints,
          tleExpr = typedExpr,
          tleType = typedType
        }
    )
