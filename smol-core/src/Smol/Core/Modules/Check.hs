{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Smol.Core.Modules.Check
  ( getModuleType,
    getModuleItemIdentifier,
    lookupModuleDefType,
    lookupModuleDef,
    filterNameDefs,
    filterTypeDefs,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Smol.Core
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module
import Smol.Core.Helpers (filterMapKeys)

lookupModuleDef ::
  Module dep (Type dep ann) ->
  DefIdentifier ->
  Maybe (Expr dep (Type dep ann))
lookupModuleDef mod' defId =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in M.lookup defId defs

lookupModuleDefType ::
  Module dep (Type dep ann) ->
  DefIdentifier ->
  Maybe (Type dep ann)
lookupModuleDefType =
  (fmap . fmap) getExprAnnotation . lookupModuleDef

-- used in logging etc, "what is this thing"
getModuleItemIdentifier :: ModuleItem ann -> Maybe DefIdentifier
-- getModuleItemIdentifier (ModuleInfix infixOp _) = Just (DIInfix infixOp)
getModuleItemIdentifier (ModuleExpression name _ _) = Just (DIName name)
getModuleItemIdentifier (ModuleDataType (DataType typeName _ _)) = Just (DIType typeName)
getModuleItemIdentifier (ModuleExport a) = getModuleItemIdentifier a
getModuleItemIdentifier (ModuleImport _) = Nothing

-- getModuleItemIdentifier (ModuleTest testName _) = Just (DITest testName)

-- return type of module as a MTRecord of dep -> monotype
-- TODO: module should probably be it's own MTModule or something
-- as we'll want to pass them about at some point I think
getModuleType :: (Monoid ann) => Module dep (Type dep ann) -> Type dep ann
getModuleType mod' =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in TRecord mempty (getExprAnnotation <$> filterNameDefs defs) 

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
