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
import Language.Mimsa.Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Backend
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- | this now accepts StoreExpression instead of expression
-- | so typechecking is merely a formality
compile ::
  Runtime Javascript ->
  Text ->
  StoreExpression Annotation ->
  Actions.ActionM (ExprHash, Set ExprHash)
compile runtime input se = do
  project <- Actions.getProject

  -- TODO: this still discards deps and typeDeps of the StoreExpression,
  -- these should be added to the project maybe? Or a top-level "get type of
  -- StoreExpression" function should be created?

  -- should typecheck - re-calc monotype for checking runtime is ok
  (ResolvedExpression mt _ _ _ _) <-
    liftEither $ getTypecheckedStoreExpression input project (storeExpression se)

  -- does runtime typecheck with expression
  liftEither (first (TypeErr input) (runtimeIsValid runtime mt))

  -- this will eventually check for things we have already transpiled to save
  -- on work
  let list = getTranspileList (prjStore project) se

  -- transpile each required file and add to outputs
  traverse_ (transpileModule (rtBackend runtime)) (list <> S.singleton se)

  -- create the index
  createIndex runtime (getStoreExpressionHash se)

  -- create the stdlib
  createStdlib (rtBackend runtime)

  -- return useful info
  let rootExprHash = getStoreExpressionHash se

  -- return all ExprHashes created
  let allHashes = S.map getStoreExpressionHash list <> S.singleton rootExprHash
  pure (rootExprHash, allHashes)

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
  js <- liftEither $ first BackendErr (outputCommonJS dataTypes se)
  let jsOutput = Actions.SaveContents (coerce js)
  Actions.appendWriteFile path filename jsOutput

-- | The index file for a given exprHash is the 'entrypoint' file
-- | that exposes the expression as a function called 'main' and imports
-- | the other files
createIndex ::
  Runtime Javascript -> ExprHash -> Actions.ActionM ()
createIndex runtime exprHash = do
  let path = Actions.SavePath (T.pack $ transpiledIndexOutputPath (rtBackend runtime))
      outputContent = Actions.SaveContents (coerce $ outputIndexFile runtime exprHash)
      filename = Actions.SaveFilename (indexFilename runtime exprHash)
  Actions.appendWriteFile path filename outputContent

-- | The stdlib is a set of functions needed to stuff like pattern matching
createStdlib :: Backend -> Actions.ActionM ()
createStdlib be = do
  let path = Actions.SavePath (T.pack $ transpiledStdlibOutputPath be)
      filename = Actions.SaveFilename (stdLibFilename be)
      outputContent = Actions.SaveContents (outputStdlib be)
  Actions.appendWriteFile path filename outputContent
