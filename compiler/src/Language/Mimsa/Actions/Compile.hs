{-# LANGUAGE OverloadedStrings #-}

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
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Backend.Backend
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Shared
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- need to make this work in a tree shape or it's going to get disgusting
-- each dep:
-- a - optimises itself (this may remove deps)
-- b - then optimises each dep
-- c - and swaps them out in deps/typeDeps of StoreExpression, creating another store expression AGAIN (oh no)
-- d - eventually we have a big pile of new StoreExpressions
-- e - which we typecheck
-- f - then transpile
--
-- feel like a-c should be a separate action as it makes sense before
-- interpreting too

getOptimisedDeps :: StoreExpression Annotation -> Actions.ActionM (Set (StoreExpression Annotation))
getOptimisedDeps se = do
  let oldExprHash = getStoreExpressionHash se

  Actions.appendMessage ("Optimising " <> prettyPrint oldExprHash)

  -- optimise bindings
  -- if new versions are made, they are `saved` in the project
  optBindings <-
    traverse
      ( getOptimisedDeps
          <=< Actions.lookupExpression
      )
      ( M.elems
          ( getBindings
              ( storeBindings se
              )
          )
      )
  -- do the same for type bindings
  optTypeBindings <-
    traverse
      ( getOptimisedDeps
          <=< Actions.lookupExpression
      )
      ( M.elems
          ( getTypeBindings
              ( storeTypeBindings se
              )
          )
      )

  -- optimise and use new deps
  optimised <-
    ( Actions.optimiseStoreExpression
        <=< Actions.useOptimisedStoreExpressionDeps
      )
      se

  let newExprHash = getStoreExpressionHash optimised

  if oldExprHash /= newExprHash
    then
      Actions.appendDocMessage
        ( "Found store expression "
            <> prettyDoc oldExprHash
            <> " and optimised to "
            <> prettyDoc newExprHash
        )
    else pure ()

  pure
    ( S.singleton optimised
        <> mconcat optBindings
        <> mconcat optTypeBindings
    )

-- | this now accepts StoreExpression instead of expression
compile ::
  Backend ->
  StoreExpression Annotation ->
  Actions.ActionM (ExprHash, Set ExprHash)
compile be se = do
  -- get optimised store expressions
  storeExprs <- getOptimisedDeps se

  -- optimise root StoreExpression
  rootStoreExpr <-
    ( Actions.optimiseStoreExpression
        <=< Actions.useOptimisedStoreExpressionDeps
      )
      se

  -- this will eventually check for things we have
  -- already transpiled to save on work
  list <-
    traverse
      Actions.annotateStoreExpressionWithTypes
      (S.toList storeExprs)

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
