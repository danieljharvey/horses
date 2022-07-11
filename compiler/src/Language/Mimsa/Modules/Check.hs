{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Check (getModuleItemIdentifier, lookupModuleDefType) where

import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Typechecker

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
getModuleItemIdentifier (ModuleTest _ _) = error "what module item identifier should a test have?"
