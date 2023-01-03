{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Compile (compileModule, compileProject) where

-- get expression
-- optimise it
-- work out what to compile for it
-- compile it to Text
-- compile stdLib to Text

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
-- import qualified Language.Mimsa.Actions.Optimise as Actions
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Modules.ToStoreExpressions as Actions
import qualified Language.Mimsa.Actions.Modules.Typecheck as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Output
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.ToStoreExprs
import Language.Mimsa.Core
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- | given a pile of StoreExpressions, turn them all into TS/JS etc
compileStoreExpressions ::
  Backend ->
  Map ExprHash (StoreExpression MonoType) ->
  Actions.ActionM (Set ExprHash)
compileStoreExpressions be typedStoreExprs = do
  -- transpile each required file and add to outputs
  traverse_
    ( \se -> do
        Actions.appendMessage ("Compiling " <> prettyPrint (getStoreExpressionHash se))
        transpileModule be se
    )
    typedStoreExprs

  -- return all ExprHashes created
  pure $ S.map getStoreExpressionHash (S.fromList $ M.elems typedStoreExprs)

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
  let path = Actions.SavePath (T.pack $ symlinkedOutputPath be)
  let filename =
        Actions.SaveFilename $
          storeExprFilename
            be
            (getStoreExpressionHash se)
            <> fileExtension be
  js <-
    liftEither $
      first
        toBackendError
        (outputStoreExpression be dataTypes (prjStore project) se)
  let jsOutput = Actions.SaveContents (coerce js)
  Actions.appendWriteFile path filename jsOutput

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
  Backend -> Map Name ExprHash -> Map ModuleName ModuleHash -> Actions.ActionM ()
createProjectIndex be exportMap moduleExportMap = do
  let indexFileContents = outputIndexFile be exportMap moduleExportMap mempty
  let path = Actions.SavePath (T.pack $ symlinkedOutputPath be)
      outputContent = Actions.SaveContents (coerce indexFileContents)
      filename = Actions.SaveFilename (projectIndexFilename be)
  Actions.appendWriteFile path filename outputContent

-- | The project index file is a `index.ts` or `index.js` that exports
-- | all the top-level items in the project
createModuleIndex ::
  ModuleHash -> Backend -> Map Name ExprHash -> Map TypeName ExprHash -> Actions.ActionM ()
createModuleIndex modHash be exportMap exportTypeMap = do
  let path = Actions.SavePath (T.pack $ symlinkedOutputPath be)
      outputContent = Actions.SaveContents (coerce $ outputIndexFile be exportMap mempty exportTypeMap)
      filename = Actions.SaveFilename (moduleFilename be modHash)
  Actions.appendWriteFile path filename outputContent

-- | get map of names -> storeexprs from compiled outputs
compiledModulesToMap :: CompiledModule ann -> Actions.ActionM (Map Name (StoreExpression ann))
compiledModulesToMap compModule =
  let findCompiled exprHash = case M.lookup exprHash (getStore $ cmStore compModule) of
        Just mod' -> pure mod'
        _ -> throwError (StoreErr (CouldNotFindStoreExpression exprHash))
   in traverse findCompiled (filterNameDefs (cmExprs compModule))

compiledModulesToTypeMap :: CompiledModule ann -> Actions.ActionM (Map TypeName (StoreExpression ann))
compiledModulesToTypeMap compModule =
  let findCompiled exprHash = case M.lookup exprHash (getStore $ cmStore compModule) of
        Just mod' -> pure mod'
        _ -> throwError (StoreErr (CouldNotFindStoreExpression exprHash))
   in traverse findCompiled (filterTypeDefs (cmExprs compModule))

--  compile every expression bound at the top level
compileProject :: Backend -> Actions.ActionM (Map Name ExprHash)
compileProject be = do
  project <- Actions.getProject

  -- include stdlib for runtime
  createStdlib be

  -- get all top-level module bindings in the project
  modules <-
    traverse
      Actions.lookupModule
      (getCurrentModules $ prjModules project)

  -- compile these too! why the hell not!
  exportModuleMap <-
    traverse
      ( \thisMod -> do
          Actions.appendMessage ("Compiling module " <> prettyPrint (snd (serializeModule thisMod)))
          (moduleHash, _, _) <- compileModule be thisMod
          pure moduleHash
      )
      modules

  -- also output a top level exports file
  createProjectIndex be mempty exportModuleMap

  -- great job
  pure mempty

-- | compile a Module and all of its dependents
compileModule ::
  Backend ->
  Module Annotation ->
  Actions.ActionM (ModuleHash, Map Name ExprHash, Map TypeName ExprHash)
compileModule be compModule = do
  -- typecheck module
  typecheckedModule <- Actions.typecheckModule (prettyPrint compModule) compModule

  -- turn it into store expressions
  compiledExps <- Actions.toStoreExpressions typecheckedModule

  -- optimise them all like a big legend -- needs them to be typechecked first
  -- though
  -- allOptimised <- Actions.optimiseAll (getStore (cmStore compiledExps))

  -- compile them all
  _ <- compileStoreExpressions be (getStore (cmStore compiledExps))

  -- create map of items to hashes for index file
  exportMap <- (fmap . fmap) getStoreExpressionHash (compiledModulesToMap compiledExps)
  exportTypeMap <- (fmap . fmap) getStoreExpressionHash (compiledModulesToTypeMap compiledExps)

  -- get hash of module for index
  let (_, moduleHash) = serializeModule compModule

  -- also output a top level exports file
  createModuleIndex moduleHash be exportMap exportTypeMap

  -- great job
  pure (moduleHash, exportMap, exportTypeMap)
