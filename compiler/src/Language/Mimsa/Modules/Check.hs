{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Check (getModuleType, getModuleItemIdentifier, lookupModuleDefType) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Utils

lookupModuleDefType :: Module (Type Annotation) -> DefIdentifier -> Maybe (Type Annotation)
lookupModuleDefType mod' defId =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in getTypeFromAnn <$> M.lookup defId defs

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
   in MTRecord mempty (getTypeFromAnn <$> filterNameDefs defs)

filterNameDefs :: Map DefIdentifier a -> Map Name a
filterNameDefs =
  filterMapKeys
    ( \case
        DIName name -> Just name
        _ -> Nothing
    )
