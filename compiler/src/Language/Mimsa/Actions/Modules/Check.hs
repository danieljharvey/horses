{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Actions.Modules.Check (checkModule) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Parse
import Language.Mimsa.Modules.Typecheck
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Utils

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
checkModule ::
  Map ModuleHash (Module Annotation) ->
  Text ->
  Actions.ActionM (Module (Type Annotation), MonoType)
checkModule modules input = do
  properMod <- parseModule modules input

  -- typecheck this module
  tcMods <- typecheckAllModules modules input properMod

  let (_, rootModuleHash) = serializeModule properMod

  tcMod <- lookupModule tcMods rootModuleHash
  pure (tcMod, getModuleType tcMod)

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
