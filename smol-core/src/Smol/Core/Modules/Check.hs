{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smol.Core.Modules.Check
  ( checkModule,
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Modules.Types.Module
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.ModuleItem
import Smol.Core.Modules.Types.Test
import Smol.Core.Modules.Types.TopLevelExpression
import Smol.Core.Transform
import Smol.Core.Typecheck.Typeclass

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

  dictModule <- passModuleDictionaries input typedModule

  pure (transformModule dictModule)

transformModule :: (Ord (dep Identifier)) => Module dep ann -> Module dep ann
transformModule inputModule =
  let transformTle tle =
        tle {tleExpr = transform (tleExpr tle)}
   in inputModule {moExpressions = transformTle <$> moExpressions inputModule}

passModuleDictionaries ::
  (MonadError (ModuleError Annotation) m) =>
  T.Text ->
  Module ResolvedDep (Type ResolvedDep Annotation) ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
passModuleDictionaries input inputModule = do
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
            (DictionaryPassingError input)
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
            (DictionaryPassingError input)
            (toDictionaryPassing dictEnv mempty typedConstraints expr)

        pure (UnitTest testName newExpr)

  newTests <- traverse passDictToTest (moTests inputModule)

  pure $ inputModule {moExpressions = newExpressions, moTests = newTests}
