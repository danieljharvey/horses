{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Smol.Core.Modules.Check
  ( checkModule,
  )
where

import Control.Monad
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Smol.Core
import Smol.Core.Helpers
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Modules.Types.DefIdentifier
import Smol.Core.Modules.Types.Module
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.ModuleItem
import Smol.Core.Modules.Types.TopLevelExpression
import Smol.Core.Typecheck.Typeclass
import Smol.Core.Typecheck.Typeclass.BuiltIns

-- this is the front door as such
checkModule ::
  (MonadError (ModuleError Annotation) m) =>
  T.Text ->
  [ModuleItem Annotation] ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
checkModule input moduleItems = do
  myModule <- moduleFromModuleParts moduleItems
  let typeclassMethods = S.fromList . M.elems . fmap tcFuncName $ builtInClasses @Annotation
  (resolvedModule, deps) <-
    modifyError ErrorInResolveDeps (resolveModuleDeps typeclassMethods myModule)
  typedModule <- typecheckModule input resolvedModule deps

  passModuleDictionaries typedModule

passModuleDictionaries ::
  (MonadError (ModuleError Annotation) m) =>
  Module ResolvedDep (Type ResolvedDep Annotation) ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
passModuleDictionaries inputModule = do
  let passDictToTopLevelExpression (ident, tle) = do
        let constraints = (fmap . fmap) getTypeAnnotation (tleConstraints tle)
            expr = tleExpr tle

        -- initial typechecking environment
        let env =
              TCEnv
                { tceVars = mempty,
                  tceDataTypes = mempty,
                  tceClasses = builtInClasses,
                  tceInstances = builtInInstances,
                  tceConstraints = constraints
                }

        tracePrettyM "old expr" expr 

        newExpr <-
          modifyError
            (DefDoesNotTypeCheck mempty (DIName ident))
            (passOuterDictionaries env constraints <=< passDictionaries env $ expr)

        tracePrettyM "new expr" newExpr

        pure $ (ident, tle {tleExpr = newExpr})

  newExpressions <- M.fromList <$> traverse passDictToTopLevelExpression (M.toList $ moExpressions inputModule)
  pure $ inputModule {moExpressions = newExpressions}
