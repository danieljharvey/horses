{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Compile where

-- get expression
-- work out what to compile for it
-- compile it to Text
-- compile stdLib to Text
-- create folders
-- save files
-- symlinking (?)

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Coerce
import Data.Foldable (traverse_)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Backend
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

compile ::
  Backend ->
  Text ->
  Expr Name Annotation ->
  Actions.ActionM ()
compile be input expr = do
  project <- Actions.getProject
  (ResolvedExpression _ se _ _ _) <-
    liftEither $ getTypecheckedStoreExpression input project expr
  -- this will eventually check for things we have already transpiled to save
  -- on work
  let list = getTranspileList (prjStore project) se
  -- transpile each required file and add to outputs
  traverse_ (transpileModule be) (list <> S.singleton se)
  -- create the index
  createIndex be (getStoreExpressionHash se)
  -- create the stdlib
  createStdlib be

-- | Each module comes from a StoreExpression
-- | and is transpiled into a folder in the store
transpileModule ::
  Backend ->
  StoreExpression Annotation ->
  Actions.ActionM ()
transpileModule be se = do
  project <- Actions.getProject
  dataTypes <- liftEither $ first StoreErr (resolveTypeDeps (prjStore project) (storeTypeBindings se))
  let path = Actions.SavePath (T.pack $ transpiledModuleOutputPath be)
  let filename = Actions.SaveFilename (moduleFilename be (getStoreExpressionHash se))
  let jsOutput = Actions.SaveContents (coerce $ outputCommonJS dataTypes se)
  Actions.appendMessage ("Writing " <> coerce path <> "...")
  Actions.appendWriteFile path filename jsOutput

-- | The index file for a given exprHash is the 'entrypoint' file
-- | that exposes the expression as a function called 'main' and imports
-- | the other files
createIndex ::
  Backend -> ExprHash -> Actions.ActionM ()
createIndex be exprHash = do
  let path = Actions.SavePath (T.pack $ transpiledIndexOutputPath be)
      outputContent = Actions.SaveContents (outputIndexFile be exprHash)
      filename = Actions.SaveFilename (indexFilename be)
  Actions.appendWriteFile path filename outputContent

-- | The stdlib is a set of functions needed to stuff like pattern matching
createStdlib :: Backend -> Actions.ActionM ()
createStdlib be = do
  let path = Actions.SavePath (T.pack $ transpiledStdlibOutputPath be)
      filename = Actions.SaveFilename (stdLibFilename be)
      outputContent = Actions.SaveContents (outputStdlib be)
  Actions.appendWriteFile path filename outputContent
