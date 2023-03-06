module Language.Mimsa.Typechecker.CreateEnv
  ( createEnv,
  )
where

import Data.Bifunctor
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.BuiltIns
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Typechecker

createEnv ::
  Map Name MonoType ->
  Map (Maybe ModuleName, TypeName) DataType ->
  Map InfixOp MonoType ->
  Map ModuleHash (Map Name MonoType) ->
  Environment
createEnv typeMap dataTypes infixTypes modTypes =
  createDepsEnv typeMap
    <> createTypesEnv dataTypes
    <> createInfixEnv infixTypes
    <> createModuleEnv modTypes

createTypesEnv :: Map (Maybe ModuleName, TypeName) DataType -> Environment
createTypesEnv dataTypes =
  Environment
    { getSchemes = mempty,
      getDataTypes = builtInDts <> dataTypes,
      getInfix = mempty,
      getTypeVarsInScope = mempty,
      getNamespacedSchemes = mempty
    }
  where
    makeDT (name, _) =
      M.singleton (Nothing, name) (DataType name mempty mempty)
    builtInDts =
      mconcat $ makeDT <$> M.toList builtInTypes

createDepsEnv :: Map Name MonoType -> Environment
createDepsEnv typeMap =
  Environment
    { getSchemes = mkSchemes typeMap,
      getDataTypes = mempty,
      getInfix = mempty,
      getTypeVarsInScope = mempty,
      getNamespacedSchemes = mempty
    }
  where
    toScheme =
      bimap
        (\(Name n) -> TVName (coerce n))
        schemeFromMonoType
    mkSchemes =
      M.fromList . fmap toScheme . M.toList

createInfixEnv :: Map InfixOp MonoType -> Environment
createInfixEnv infixTypes =
  Environment
    { getSchemes = mempty,
      getDataTypes = mempty,
      getInfix = schemeFromMonoType <$> infixTypes,
      getTypeVarsInScope = mempty,
      getNamespacedSchemes = mempty
    }

createModuleEnv :: Map ModuleHash (Map Name MonoType) -> Environment
createModuleEnv modTypes =
  mempty
    { getNamespacedSchemes = (fmap . fmap) schemeFromMonoType modTypes
    }

-- | Make all free variables polymorphic so that we get a fresh version of
-- everything each time
schemeFromMonoType :: MonoType -> Scheme
schemeFromMonoType mt = Scheme (S.toList $ freeTypeVars mt) mt
