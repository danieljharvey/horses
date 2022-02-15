{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handlers
  ( ProjectData (..),
    fromActionM,
    eitherFromActionM,
    projectDataHandler,
    loadProjectHandler,
    saveExprHandler,
    saveFileHandler,
    findExprHandler,
    projectFromExpressionHandler,
    resolveStoreExpressionHandler,
    readStoreHandler,
    writeStoreHandler,
    useSwapsHandler,
    runTestsHandler,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi (ToSchema)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
  ( getTypeMap,
    resolveStoreExpression,
  )
import Language.Mimsa.Interpreter.UseSwaps
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.Persistence
import Language.Mimsa.Project.Usages
import Language.Mimsa.Project.Versions
import Language.Mimsa.Store
import Language.Mimsa.Tests.Test
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker
import Servant
import Server.Helpers
import Server.Types

-----
-- Commonly used functionality, lifted into Servant's Handler type
-----

fromActionM ::
  MimsaEnvironment ->
  ProjectHash ->
  Actions.ActionM a ->
  Handler (Project Annotation, a)
fromActionM mimsaEnv projectHash action = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  case Actions.run project action of
    Left e -> throwError (to400Error e)
    Right (newProject, outcomes, a) -> do
      traverse_ (saveExprHandler mimsaEnv) (Actions.storeExpressionsFromOutcomes outcomes)
      -- TODO: return these as a list of Text to show up in a toast in app
      -- traverse_ replOutput (Actions.messagesFromOutcomes outcomes)
      traverse_ (saveFileHandler mimsaEnv) (Actions.writeFilesFromOutcomes outcomes)

      pure (newProject, a)

eitherFromActionM ::
  MimsaEnvironment ->
  ProjectHash ->
  Actions.ActionM a ->
  Handler (Either (Error Annotation) (Project Annotation, a))
eitherFromActionM mimsaEnv projectHash action = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  case Actions.run project action of
    Left e -> pure (Left e)
    Right (newProject, outcomes, a) -> do
      traverse_ (saveExprHandler mimsaEnv) (Actions.storeExpressionsFromOutcomes outcomes)
      -- TODO: return these as a list of Text to show up in a toast in app
      -- traverse_ replOutput (Actions.messagesFromOutcomes outcomes)
      traverse_ (saveFileHandler mimsaEnv) (Actions.writeFilesFromOutcomes outcomes)

      pure (Right (newProject, a))

outputBindings :: Project a -> Map Name Text
outputBindings project =
  prettyPrint
    <$> getBindings
      ( getCurrentBindings
          (prjBindings project)
      )

outputTypeBindings :: Project a -> Map TyCon Text
outputTypeBindings project =
  prettyPrint
    <$> getTypeBindings
      (getCurrentTypeBindings (prjTypeBindings project))

-- | Version of a given binding
-- number, exprHash, usages elsewhere
data BindingVersion = BindingVersion
  { bvNumber :: Int,
    bvExprHash :: ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

toExprUsages :: Set Usage -> [ExprUsage]
toExprUsages =
  fmap
    ( \case
        Transient name exprHash ->
          ExprUsage exprHash (coerce name) False
        Direct name exprHash ->
          ExprUsage exprHash (coerce name) True
    )
    . S.toList

data ExprUsage = ExprUsage
  { euExprHash :: ExprHash,
    euName :: Text,
    euIsDirect :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

-- | Current state of the project
-- should contain no exprs
data ProjectData = ProjectData
  { pdHash :: ProjectHash,
    pdBindings :: Map Name Text,
    pdTypeBindings :: Map TyCon Text,
    pdVersions :: Map Name (NE.NonEmpty BindingVersion),
    pdUsages :: Map ExprHash [ExprUsage]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

-- read the store from mutable var to stop repeated loading of exprs
readStoreHandler :: MimsaEnvironment -> Handler (Store Annotation)
readStoreHandler mimsaEnv = do
  liftIO $ STM.atomically $ STM.readTVar (mutableStore mimsaEnv)

writeStoreHandler :: MimsaEnvironment -> Store Annotation -> Handler ()
writeStoreHandler mimsaEnv store' = do
  liftIO $
    STM.atomically $
      STM.modifyTVar
        (mutableStore mimsaEnv)
        (<> store')

versionsForBinding ::
  (Printer ann, Show ann) =>
  Project ann ->
  Name ->
  Handler (NE.NonEmpty BindingVersion)
versionsForBinding prj name = do
  versions <- handleEither InternalError (findVersionsSimple prj name)
  pure $ uncurry BindingVersion <$> versions

usagesForExprHash :: Project ann -> ExprHash -> Handler (Set Usage)
usagesForExprHash prj exprHash =
  handleEither InternalError (findUsages prj exprHash)

-- given a new Project, save it and return the hash and bindings
projectDataHandler ::
  (Printer ann, Show ann) =>
  MimsaEnvironment ->
  Project ann ->
  Handler ProjectData
projectDataHandler mimsaEnv project = do
  -- get all versions of a binding and numbers them
  let nameMap =
        M.mapWithKey const
          <$> getBindings . getCurrentBindings . prjBindings
          $ project
  versions <- traverse (versionsForBinding project) nameMap
  -- list usages of each exprhash in the project
  let hashesMap = M.mapWithKey const <$> getStore . prjStore $ project
  usages <- traverse (usagesForExprHash project) hashesMap

  -- save project file
  projHash <-
    handleMimsaM
      (mimsaConfig mimsaEnv)
      InternalError
      (saveProjectInStore project)

  pure $
    ProjectData
      projHash
      (outputBindings project)
      (outputTypeBindings project)
      versions
      (toExprUsages <$> usages)

-- given a project hash, find the project
loadProjectHandler ::
  MimsaEnvironment ->
  Store Annotation ->
  ProjectHash ->
  Handler (Project Annotation)
loadProjectHandler mimsaEnv store' hash =
  handleMimsaM (mimsaConfig mimsaEnv) UserError (loadProjectFromHash store' hash)

saveExprHandler ::
  MimsaEnvironment ->
  StoreExpression ann ->
  Handler ExprHash
saveExprHandler mimsaEnv se =
  handleMimsaM (mimsaConfig mimsaEnv) InternalError (saveExpr se)

saveFileHandler ::
  MimsaEnvironment ->
  (Actions.SavePath, Actions.SaveFilename, Actions.SaveContents) ->
  Handler ()
saveFileHandler mimsaEnv saveInfo =
  handleMimsaM (mimsaConfig mimsaEnv) InternalError (saveFile saveInfo)

resolveStoreExpressionHandler ::
  Project Annotation ->
  StoreExpression Annotation ->
  Handler (ResolvedExpression Annotation)
resolveStoreExpressionHandler prj se = do
  typeMap <- handleEither InternalError (Actions.getTypeMap prj)
  handleEither UserError $
    Actions.resolveStoreExpression
      (prjStore prj)
      typeMap
      (prettyPrint (storeExpression se))
      se

findExprHandler ::
  Project Annotation ->
  ExprHash ->
  Handler (StoreExpression Annotation)
findExprHandler project exprHash' =
  handleEither InternalError $
    case lookupExprHash project exprHash' of
      Nothing -> Left ("Could not find exprhash!" :: Text)
      Just a -> Right a

-- given an exprhash, load a project containing its dependents
projectFromExpressionHandler ::
  MimsaEnvironment ->
  ExprHash ->
  Handler (StoreExpression Annotation, ProjectData, Project Annotation)
projectFromExpressionHandler mimsaEnv exprHash = do
  -- load store with just items for storeExpr in
  store <- storeFromExprHashHandler mimsaEnv exprHash
  -- create a project with this store
  let project = fromStore store $> mempty
  -- find the storeExpr we want in the store
  storeExpr <- findExprHandler project exprHash
  -- add deps of storeExpr to project
  let projectWithStoreExpr = project <> fromStoreExpressionDeps storeExpr
  -- save shit
  pd <- projectDataHandler mimsaEnv projectWithStoreExpr
  writeStoreHandler mimsaEnv (prjStore projectWithStoreExpr)
  pure (storeExpr, pd, projectWithStoreExpr)

storeFromExprHashHandler ::
  MimsaEnvironment ->
  ExprHash ->
  Handler (Store ())
storeFromExprHashHandler mimsaEnv exprHash =
  handleMimsaM
    (mimsaConfig mimsaEnv)
    UserError
    (recursiveLoadBoundExpressions mempty (S.singleton exprHash))

useSwapsHandler :: Swaps -> Expr Variable MonoType -> Handler (Expr Name MonoType)
useSwapsHandler swaps expr =
  handleEither UserError (useSwaps swaps expr)

runTestsHandler ::
  MimsaEnvironment ->
  Project Annotation ->
  [Test] ->
  Handler [TestResult Annotation]
runTestsHandler mimsaEnv project tests = do
  handleMimsaM
    (mimsaConfig mimsaEnv)
    InternalError
    (traverse (runTests project) tests)
