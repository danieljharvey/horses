{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Compile (compileStoreExpression, compileProject) where

-- get expression
-- optimise it
-- work out what to compile for it
-- compile it to Text
-- compile stdLib to Text

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Backend.Output
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- | this now accepts StoreExpression instead of expression
compileStoreExpression ::
  Backend ->
  StoreExpression Annotation ->
  Actions.ActionM (ExprHash, Set ExprHash)
compileStoreExpression be se = do
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
  hashes <-
    compileStoreExpressions be storeExprs

  -- create the index
  createIndex be (getStoreExpressionHash rootStoreExpr)

  -- include stdlib for runtime
  createStdlib be

  -- return useful info
  let rootExprHash = getStoreExpressionHash rootStoreExpr

  -- return all ExprHashes created
  let allHashes =
        hashes
          <> S.singleton rootExprHash

  pure (rootExprHash, allHashes)

-- | given a pile of StoreExpressions, turn them all into TS/JS etc
compileStoreExpressions ::
  Backend ->
  Map ExprHash (StoreExpression Annotation) ->
  Actions.ActionM (Set ExprHash)
compileStoreExpressions be storeExprs = do
  -- this will eventually check for things we have
  -- already transpiled to save on work
  list <-
    traverse
      Actions.annotateStoreExpressionWithTypes
      (M.elems storeExprs)

  -- transpile each required file and add to outputs
  traverse_
    ( \se -> do
        Actions.appendMessage ("Compiling " <> prettyPrint (getStoreExpressionHash se))
        transpileModule be se
    )
    list

  -- return all ExprHashes created
  pure $ S.map getStoreExpressionHash (S.fromList list)

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
  let path = Actions.SavePath (T.pack $ symlinkedOutputPath be)
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
  let path = Actions.SavePath (T.pack $ symlinkedOutputPath be)
      outputContent = Actions.SaveContents (coerce $ outputIndexFile be (M.singleton "main" exprHash))
      filename = Actions.SaveFilename (indexFilename be exprHash)
  Actions.appendWriteFile path filename outputContent

-- The stdlib is a set of functions needed to stuff like pattern matching
createStdlib :: Backend -> Actions.ActionM ()
createStdlib be = do
  let path = Actions.SavePath (T.pack $ symlinkedOutputPath be)
      filename = Actions.SaveFilename (stdlibFilename be <> fileExtension be)
      outputContent = Actions.SaveContents (outputStdlib be)
  Actions.appendWriteFile path filename outputContent

-- | The project index file is a `index.ts` or `index.js` that exports
-- | all the top-level items in the project
createProjectIndex ::
  Backend -> Map Name ExprHash -> Actions.ActionM ()
createProjectIndex be exportMap = do
  let path = Actions.SavePath (T.pack $ symlinkedOutputPath be)
      outputContent = Actions.SaveContents (coerce $ outputIndexFile be exportMap)
      filename = Actions.SaveFilename (projectIndexFilename be)
  Actions.appendWriteFile path filename outputContent

--  compile every expression bound at the top level
compileProject :: Backend -> Actions.ActionM (Map Name ExprHash)
compileProject be = do
  project <- Actions.getProject

  -- get store expressions for all the top level bindings in the project
  storeExprs <-
    traverse
      Actions.lookupExpression
      (getBindings . getCurrentBindings . prjBindings $ project)

  -- get dependencies of all the StoreExpressions
  depsSe <- mconcat <$> traverse Actions.getDepsForStoreExpression (M.elems storeExprs)

  -- compile them all
  -- TODO: we are not optimising these, perhaps we should
  _ <- compileStoreExpressions be (fst <$> depsSe)

  let exportMap = getStoreExpressionHash <$> storeExprs

  -- include stdlib for runtime
  createStdlib be

  -- also output a top level exports file
  createProjectIndex be exportMap

  -- great job
  pure exportMap
