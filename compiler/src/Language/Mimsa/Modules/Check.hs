{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Check (checkModule, getModuleItemIdentifier, lookupModuleDefType) where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Parse
import Language.Mimsa.Modules.Typecheck
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Utils

checkModule ::
  Text ->
  Map ModuleHash (Module Annotation) ->
  Either (Error Annotation) (Module (Type Annotation), MonoType)
checkModule input modules = runCheck input modules (checkModule' input)

-- | This is where we load a file and check that it is "OK" as such
--  so far this entails:
--  1. parsing it
--  2. ordering things
--  3. typechecking everything
--
--  so far the features in modules are
--  1. definitions of values
--  2. types of values
--  3. definitions of datatypes
--  4. exports
--  5. imports
--  6. infix
--
--  soon there will also need to be
--  1. tests
--  2. property tests
--  3. metadata / comments etc?
checkModule' :: Text -> CheckM (Module (Type Annotation), MonoType)
checkModule' input = do
  properMod <- parseModule' input

  -- typecheck this module
  tcMods <- typecheckAllModules properMod

  let (_, rootModuleHash) = serializeModule properMod

  case M.lookup rootModuleHash tcMods of
    Nothing -> throwError (ModuleErr $ MissingModule rootModuleHash)
    Just tcMod -> pure (tcMod, getModuleType tcMod)

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

lookupModuleDefType :: Module (Type Annotation) -> DefIdentifier -> Maybe (Type Annotation)
lookupModuleDefType mod' defId =
  let defs =
        M.filterWithKey
          (\k _ -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
   in getTypeFromAnn <$> M.lookup defId defs

filterNameDefs :: Map DefIdentifier a -> Map Name a
filterNameDefs =
  filterMapKeys
    ( \case
        DIName name -> Just name
        _ -> Nothing
    )

-- used in logging etc, "what is this thing"
getModuleItemIdentifier :: ModuleItem ann -> Maybe DefIdentifier
getModuleItemIdentifier (ModuleInfix infixOp _) = Just (DIInfix infixOp)
getModuleItemIdentifier (ModuleExpression name _ _) = Just (DIName name)
getModuleItemIdentifier (ModuleDataType (DataType typeName _ _)) = Just (DIType typeName)
getModuleItemIdentifier (ModuleExport a) = getModuleItemIdentifier a
getModuleItemIdentifier (ModuleImport _) = Nothing
getModuleItemIdentifier (ModuleTest _ _) = error "what module item identifier should a test have?"
