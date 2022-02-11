module Language.Mimsa.Actions.Compile where

-- get expression
-- optimise it
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
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
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
  optSe <- Actions.optimiseStoreExpression se
  liftEither $ Actions.typecheckStoreExpression (prjStore project) optSe

-- | this now accepts StoreExpression instead of expression
compile ::
  Backend ->
  StoreExpression Annotation ->
  Actions.ActionM (ExprHash, Set ExprHash)
compile be se = do
  project <- Actions.getProject

  -- optimise expression
  optSe <- Actions.optimiseStoreExpression se

  -- get type of StoreExpression
  typedMt <- typecheckStoreExpression optSe

  -- this will eventually check for things we have already transpiled to save
  -- on work
  list <-
    traverse
      typecheckStoreExpression
      (S.toList $ getTranspileList (prjStore project) optSe)

  -- transpile each required file and add to outputs
  traverse_ (transpileModule be) (list <> pure typedMt)

  -- create the index
  createIndex be (getStoreExpressionHash optSe)

  -- include stdlib for runtime
  createStdlib be

  -- return useful info
  let rootExprHash = getStoreExpressionHash optSe

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
  Backend -> ExprHash -> Actions.ActionM ()
createIndex be exprHash = do
  let path = Actions.SavePath (T.pack $ transpiledIndexOutputPath be)
      outputContent = Actions.SaveContents (coerce $ outputIndexFile be exprHash)
      filename = Actions.SaveFilename (indexFilename be exprHash)
  Actions.appendWriteFile path filename outputContent

-- The stdlib is a set of functions needed to stuff like pattern matching
createStdlib :: Backend -> Actions.ActionM ()
createStdlib be = do
  let path = Actions.SavePath (T.pack $ transpiledStdlibOutputPath be)
      filename = Actions.SaveFilename (stdlibFilename be <> fileExtension be)
      outputContent = Actions.SaveContents (outputStdlib be)
  Actions.appendWriteFile path filename outputContent
