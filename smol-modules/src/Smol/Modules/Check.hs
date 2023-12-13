{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smol.Modules.Check
  ( checkModule,
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Smol.Core
import Smol.Core.Annotations
import Smol.Core.Helpers (tracePrettyId)
import Smol.Modules.FromParts
import Smol.Modules.Helpers
import Smol.Modules.ResolveDeps
import Smol.Modules.Typecheck
import Smol.Modules.Types.Module
import Smol.Modules.Types.ModuleError
import Smol.Modules.Types.ModuleItem
import Smol.Modules.Types.Test
import Smol.Modules.Types.TopLevelExpression
import Smol.Typecheck.Typeclass
import Smol.Typecheck.Types

-- this is the front door as such
checkModule ::
  (MonadError (ModuleError Annotation) m) =>
  T.Text ->
  [ModuleItem Annotation] ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
checkModule input moduleItems = do
  myModule <- moduleFromModuleParts moduleItems

  let classes = resolveTypeclass <$> moClasses myModule
      typeclassMethods = S.fromList . M.elems . fmap tcFuncName $ classes

  (resolvedModule, deps) <-
    modifyError ErrorInResolveDeps (resolveModuleDeps typeclassMethods myModule)

  typedModule <- typecheckModule input resolvedModule deps

  dictModule <- passModuleDictionaries typedModule

  pure (transformModule dictModule)

transformModule :: (Ord ann, Printer (dep TypeName), Printer (dep Identifier), Printer (dep Constructor), Ord (dep Identifier)) => Module dep ann -> Module dep ann
transformModule inputModule =
  let transformTle tle =
        tle
          { tleExpr =
              tracePrettyId
                "after"
                (transform (tracePrettyId "before" $ tleExpr tle))
          }
   in inputModule {moExpressions = transformTle <$> moExpressions inputModule}

passModuleDictionaries ::
  (MonadError (ModuleError Annotation) m) =>
  Module ResolvedDep (Type ResolvedDep Annotation) ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
passModuleDictionaries inputModule = do
  let env = envFromTypecheckedModule inputModule

  let passDictToTopLevelExpression (ident, tle) = do
        let constraints = constraintsFromTLE tle
            expr = tleExpr tle

        let typedConstraints = addTypesToConstraint <$> constraints
            dictEnv =
              ToDictEnv
                { tdeClasses = tceClasses env,
                  tdeInstances = moInstances inputModule,
                  tdeVars = getVarsInScope inputModule
                }
        newExpr <-
          modifyError
            DictionaryPassingError
            (toDictionaryPassing dictEnv mempty typedConstraints expr)

        pure (ident, tle {tleExpr = newExpr})

  newExpressions <- M.fromList <$> traverse passDictToTopLevelExpression (M.toList $ moExpressions inputModule)

  let passDictToTest (UnitTest testName expr) = do
        let constraints = mempty -- test should have no constraints to satisfy
        let typedConstraints = addTypesToConstraint <$> constraints
            dictEnv =
              ToDictEnv
                { tdeClasses = tceClasses env,
                  tdeInstances = moInstances inputModule,
                  tdeVars = getVarsInScope inputModule
                }
        newExpr <-
          modifyError
            DictionaryPassingError
            (toDictionaryPassing dictEnv mempty typedConstraints expr)

        pure (UnitTest testName newExpr)

  newTests <- traverse passDictToTest (moTests inputModule)

  pure $ inputModule {moExpressions = newExpressions, moTests = newTests}

---

constraintsFromTLE ::
  TopLevelExpression ResolvedDep (Type ResolvedDep ann) ->
  [Constraint ResolvedDep ann]
constraintsFromTLE tle =
  (fmap . fmap) getTypeAnnotation (tleConstraints tle)

-- get input for typechecker from module
getVarsInScope ::
  Module ResolvedDep (Type ResolvedDep ann) ->
  M.Map (ResolvedDep Identifier) ([Constraint ResolvedDep ann], ResolvedType ann)
getVarsInScope =
  M.fromList
    . fmap go
    . M.toList
    . moExpressions
  where
    go (ident, tle) =
      ( LocalDefinition ident,
        (constraintsFromTLE tle, getExprAnnotation (tleExpr tle))
      )

-- make a typechecking env from a module
-- this means throwing away all the types which seems silly
envFromTypecheckedModule :: (Ord ann, Monoid ann) => Module ResolvedDep (Type ResolvedDep ann) -> TCEnv ann
envFromTypecheckedModule inputModule =
  let instances =
        mapKey (fmap (const mempty))
          . (fmap . fmap) getTypeAnnotation
          . moInstances
          $ inputModule

      classes = (fmap . fmap) getTypeAnnotation (moClasses inputModule)

      dataTypes =
        (fmap . fmap)
          getTypeAnnotation
          (M.mapKeys LocalDefinition (moDataTypes inputModule))
   in TCEnv
        { tceVars = getVarsInScope inputModule,
          tceDataTypes = dataTypes,
          tceInstances = instances,
          tceClasses = classes,
          tceConstraints = mempty
        }
