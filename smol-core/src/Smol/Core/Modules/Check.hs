{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Smol.Core.Modules.Check
  ( getModuleItemIdentifier,
    lookupModuleDefType,
    lookupModuleDef,
    filterNameDefs,
    filterTypeDefs,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Smol.Core
import Smol.Core.Helpers (filterMapKeys)
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleItem
import Smol.Core.Types.Module.TopLevelExpression

lookupModuleDef ::
  Module dep (Type dep ann) ->
  Identifier ->
  Maybe (TopLevelExpression dep (Type dep ann))
lookupModuleDef mod' defId =
  M.lookup defId (moExpressions mod')

lookupModuleDefType ::
  Module dep (Type dep ann) ->
  Identifier ->
  Maybe (Type dep ann)
lookupModuleDefType =
  (fmap . fmap) (getExprAnnotation . tleExpr) . lookupModuleDef

-- used in logging etc, "what is this thing"
getModuleItemIdentifier :: ModuleItem ann -> Maybe DefIdentifier
getModuleItemIdentifier (ModuleExpression name _ _) = Just (DIName name)
getModuleItemIdentifier (ModuleDataType (DataType typeName _ _)) = Just (DIType typeName)
getModuleItemIdentifier (ModuleExpressionType name _) = Just (DIName name)
getModuleItemIdentifier (ModuleTest testName _) = Just (DITest testName)

filterNameDefs :: Map DefIdentifier a -> Map Identifier a
filterNameDefs =
  filterMapKeys
    ( \case
        DIName name -> Just name
        _ -> Nothing
    )

filterTypeDefs :: Map DefIdentifier a -> Map TypeName a
filterTypeDefs =
  filterMapKeys
    ( \case
        DIType typeName -> Just typeName
        _ -> Nothing
    )
