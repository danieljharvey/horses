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
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Backend.Backend
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Shared
import Language.Mimsa.ExprUtils
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

typecheckStoreExpression :: StoreExpression Annotation -> Actions.ActionM (StoreExpression MonoType)
typecheckStoreExpression se = do
  project <- Actions.getProject
  liftEither $ Actions.typecheckStoreExpression (prjStore project) se

-- | this now accepts StoreExpression instead of expression
compile ::
  Runtime Text ->
  Text ->
  StoreExpression Annotation ->
  Actions.ActionM (ExprHash, Set ExprHash)
compile runtime input se = do
  project <- Actions.getProject

  -- get type of StoreExpression
  typedMt <- typecheckStoreExpression se

  let mt = getAnnotation (storeExpression typedMt)

  -- does runtime typecheck with expression
  liftEither (first (TypeErr input) (runtimeIsValid runtime mt))

  -- this will eventually check for things we have already transpiled to save
  -- on work
  list <-
    traverse
      typecheckStoreExpression
      (S.toList $ getTranspileList (prjStore project) se)

  -- transpile each required file and add to outputs
  traverse_ (transpileModule (rtBackend runtime)) (list <> pure typedMt)

  -- create the index
  createIndex runtime (getStoreExpressionHash se)

  -- return useful info
  let rootExprHash = getStoreExpressionHash se

  -- return all ExprHashes created
  let allHashes =
        S.map getStoreExpressionHash (S.fromList list)
          <> S.singleton rootExprHash
  pure (rootExprHash, allHashes)

toBackendError :: BackendError MonoType -> Error Annotation
toBackendError err = BackendErr (getAnnotationForType <$> err)

-- | Each module comes from a StoreExpression
-- | and is transpiled into a folder in the store
transpileModule ::
  Backend ->
  StoreExpression MonoType ->
  Actions.ActionM ()
transpileModule be se = do
  project <- Actions.getProject
  dataTypes <-
    liftEither $
      first
        StoreErr
        (resolveTypeDeps (prjStore project) (storeTypeBindings se))
  let monoType = getAnnotation (storeExpression se)
  let path = Actions.SavePath (T.pack $ transpiledModuleOutputPath be)
  let filename =
        Actions.SaveFilename $
          moduleFilename
            be
            (getStoreExpressionHash se)
            <> fileExtension be
  js <-
    liftEither $
      first
        toBackendError
        (outputStoreExpression be dataTypes (prjStore project) monoType se)
  let jsOutput = Actions.SaveContents (coerce js)
  Actions.appendWriteFile path filename jsOutput

-- | The index file for a given exprHash is the 'entrypoint' file
-- | that exposes the expression as a function called 'main' and imports
-- | the other files
createIndex ::
  Runtime Text -> ExprHash -> Actions.ActionM ()
createIndex runtime exprHash = do
  let be = rtBackend runtime
      path = Actions.SavePath (T.pack $ transpiledIndexOutputPath be)
      outputContent = Actions.SaveContents (coerce $ outputIndexFile be runtime exprHash)
      filename = Actions.SaveFilename (indexFilename runtime exprHash)
  Actions.appendWriteFile path filename outputContent
