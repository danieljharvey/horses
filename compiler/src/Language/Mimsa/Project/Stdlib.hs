{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Stdlib
  ( buildStdlib,
    stdlib,
    addType,
    addBinding,
    addModule,
    removeBinding,
    allFns,
  )
where

import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Modules.Bind as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.RemoveBinding as Actions
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Prelude
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project

buildStdlib :: Either (Error Annotation) (Project Annotation)
buildStdlib =
  Actions.run mempty allFns >>= \(proj, _, _) -> pure proj

allFns :: Actions.ActionM ()
allFns = do
  modules

-- | these are files in /static/modules folder that we import
modules :: Actions.ActionM ()
modules = do
  maybeHash <-
    addModule "Maybe" mempty maybeInput
  preludeHash <-
    addModule "Prelude" mempty preludeInput
  arrayHash <-
    addModule "Array" (M.fromList [("Maybe", maybeHash)]) arrayInput
  nonEmptyArrayHash <-
    addModule "NonEmptyArray" (M.fromList [("Array", arrayHash)]) nonEmptyArrayInput
  _ <-
    addModule "Either" mempty eitherInput
  _ <-
    addModule "Reader" (M.fromList [("Prelude", preludeHash)]) readerInput
  _ <-
    addModule "These" mempty theseInput
  _ <-
    addModule "Monoid" (M.fromList [("Array", arrayHash), ("Prelude", preludeHash), ("Maybe", maybeHash)]) monoidInput
  _ <-
    addModule "State" (M.fromList [("Prelude", preludeHash)]) stateInput
  _ <-
    addModule "String" (M.fromList [("Array", arrayHash)]) stringInput
  _ <-
    addModule
      "Parser"
      ( M.fromList
          [ ("Maybe", maybeHash),
            ("Prelude", preludeHash),
            ("NonEmptyArray", nonEmptyArrayHash)
          ]
      )
      parserInput
  _ <-
    addModule "Tree" mempty treeInput
  pure ()

addType :: Text -> Actions.ActionM ()
addType t = do
  dt <- Actions.parseDataType t
  Actions.bindType t dt $> ()

addBinding :: Name -> Text -> Actions.ActionM ()
addBinding name b = do
  expr <- Actions.parseExpr b
  _ <- Actions.bindExpression expr name b
  pure ()

-- | add a module to the stdlib, adding some named imports
addModule :: ModuleName -> Map ModuleName ModuleHash -> Text -> Actions.ActionM ModuleHash
addModule moduleName deps input = do
  mod' <- Actions.parseModule input
  let modWithImports = mod' {moNamedImports = moNamedImports mod' <> deps}
  _ <- Actions.bindModule modWithImports moduleName (prettyPrint modWithImports)
  pure (snd $ serializeModule modWithImports)

removeBinding :: Name -> Actions.ActionM ()
removeBinding = Actions.removeBinding

fromRight :: (Printer e) => Either e a -> a
fromRight = \case
  Left e -> error (T.unpack (prettyPrint e))
  Right a -> a

stdlib :: Project Annotation
stdlib = fromRight buildStdlib
