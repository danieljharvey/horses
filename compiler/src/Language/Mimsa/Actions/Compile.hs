module Language.Mimsa.Actions.Compile (compile) where

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
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
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

-- | this now accepts StoreExpression instead of expression
compile ::
  Backend ->
  StoreExpression Annotation ->
  Actions.ActionM (ExprHash, Set ExprHash)
compile be se = do
  -- get dependencies of StoreExpression
  depsSe <- Actions.getDepsForStoreExpression se

  -- optimise them all like a big legend
  storeExprs <- Actions.optimiseAll (fst <$> depsSe)

  -- get new root StoreExpression (it may be different due to optimisation)
  rootStoreExpr <- case M.lookup (getStoreExpressionHash se) storeExprs of
    Just re -> pure re
    _ -> throwError (StoreErr (CouldNotFindStoreExpression (getStoreExpressionHash se)))

  -- this will eventually check for things we have
  -- already transpiled to save on work
  list <-
    traverse
      Actions.annotateStoreExpressionWithTypes
      (M.elems storeExprs)

  -- transpile each required file and add to outputs
  traverse_ (transpileModule be) list

  -- create the index
  createIndex be (getStoreExpressionHash rootStoreExpr)

  -- include stdlib for runtime
  createStdlib be

  -- return useful info
  let rootExprHash = getStoreExpressionHash rootStoreExpr

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
