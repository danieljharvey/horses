{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Check
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
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.Elaborate

lookupModuleDef :: Module (Type Annotation) -> DefIdentifier -> Maybe (Expr Name (Type Annotation))
lookupModuleDef mod' defId =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in M.lookup defId defs

lookupModuleDefType :: Module (Type Annotation) -> DefIdentifier -> Maybe (Type Annotation)
lookupModuleDefType = (fmap . fmap) getTypeFromAnn . lookupModuleDef

-- used in logging etc, "what is this thing"
getModuleItemIdentifier :: ModuleItem ann -> Maybe DefIdentifier
getModuleItemIdentifier (ModuleInfix infixOp _) = Just (DIInfix infixOp)
getModuleItemIdentifier (ModuleExpression name _ _) = Just (DIName name)
getModuleItemIdentifier (ModuleDataType (DataType typeName _ _)) = Just (DIType typeName)
getModuleItemIdentifier (ModuleExport a) = getModuleItemIdentifier a
getModuleItemIdentifier (ModuleImport _) = Nothing
getModuleItemIdentifier (ModuleTest testName _) = Just (DITest testName)

-- return type of module as a MTRecord of dep -> monotype
-- TODO: module should probably be it's own MTModule or something
-- as we'll want to pass them about at some point I think
getModuleType :: Module (Type Annotation) -> Type Annotation
getModuleType mod' =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in MTRecord mempty (getTypeFromAnn <$> filterNameDefs defs) Nothing

filterNameDefs :: Map DefIdentifier a -> Map Name a
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
