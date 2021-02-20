{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Server.Handlers
  ( ProjectData (..),
    UnitTestData (..),
    ExpressionData (..),
    fromActionM,
    projectDataHandler,
    expressionDataHandler,
    loadProjectHandler,
    evaluateTextHandler,
    createNewUnitTestsHandler,
    parseHandler,
    saveExprHandler,
    interpretHandler,
    findExprHandler,
    resolveStoreExpressionHandler,
    readStoreHandler,
    writeStoreHandler,
    createUnitTestHandler,
    mkUnitTestData,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Actions
  ( evaluateText,
    getTypeMap,
    resolveStoreExpression,
  )
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Monad
import Language.Mimsa.Parser (parseExprAndFormatError)
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Server.Helpers
import Language.Mimsa.Server.Types
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker
import Servant

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
      let storeExprs = Actions.storeExpressionsFromOutcomes outcomes
      traverse_ (saveExprHandler mimsaEnv) storeExprs

      pure (newProject, a)

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

data ProjectData = ProjectData
  { pdHash :: ProjectHash,
    pdBindings :: Map Name Text,
    pdTypeBindings :: Map TyCon Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

data UnitTestData = UnitTestData
  { utdTestName :: Text,
    utdTestSuccess :: Bool,
    utdBindings :: Map Name Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

mkUnitTestData :: Project ann -> UnitTest -> UnitTestData
mkUnitTestData project unitTest = do
  let getDep = (`findBindingNameForExprHash` project)
  let depMap = mconcat (getDep <$> S.toList (utDeps unitTest))
  UnitTestData
    (coerce $ utName unitTest)
    (coerce $ utSuccess unitTest)
    (coerce <$> depMap)

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

-- given a new Project, save it and return the hash and bindings
projectDataHandler :: MimsaEnvironment -> Project ann -> Handler ProjectData
projectDataHandler mimsaEnv env = do
  projHash <- handleMimsaM (mimsaConfig mimsaEnv) InternalError (saveProjectInStore env)
  pure $
    ProjectData
      projHash
      (outputBindings env)
      (outputTypeBindings env)

data ExpressionData = ExpressionData
  { edHash :: Text,
    edPretty :: Text,
    edType :: Text,
    edBindings :: Map Name Text,
    edTypeBindings :: Map TyCon Text,
    edUnitTests :: [UnitTestData]
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

expressionDataHandler ::
  Project Annotation ->
  StoreExpression Annotation ->
  MonoType ->
  Handler ExpressionData
expressionDataHandler project se mt = do
  let exprHash = getStoreExpressionHash se
      tests =
        mkUnitTestData project
          <$> M.elems
            (getTestsForExprHash project exprHash)
  pure $
    ExpressionData
      (prettyPrint exprHash)
      (prettyPrint (storeExpression se))
      (prettyPrint mt)
      (prettyPrint <$> getBindings (storeBindings se))
      (prettyPrint <$> getTypeBindings (storeTypeBindings se))
      tests

-- given a project hash, find the project
loadProjectHandler ::
  MimsaEnvironment ->
  Store Annotation ->
  ProjectHash ->
  Handler (Project Annotation)
loadProjectHandler mimsaEnv store' hash =
  handleMimsaM (mimsaConfig mimsaEnv) UserError (loadProjectFromHash store' hash)

evaluateTextHandler ::
  Project Annotation ->
  Text ->
  Handler (ResolvedExpression Annotation)
evaluateTextHandler project code = handleEither UserError (evaluateText project code)

parseHandler :: Text -> Handler (Expr Name Annotation)
parseHandler input =
  let wrapError :: Text -> Error Annotation
      wrapError = ParseError
   in handleEither
        UserError
        (first wrapError (parseExprAndFormatError input))

saveExprHandler :: MimsaEnvironment -> StoreExpression ann -> Handler ExprHash
saveExprHandler mimsaEnv se =
  handleMimsaM (mimsaConfig mimsaEnv) InternalError (saveExpr se)

interpretHandler ::
  Scope Annotation ->
  Swaps ->
  Expr Variable Annotation ->
  Handler (Expr Variable Annotation)
interpretHandler scope' swaps' expr' =
  handleEither InternalError (interpret scope' swaps' expr')

resolveStoreExpressionHandler ::
  Project Annotation ->
  StoreExpression Annotation ->
  Handler (ResolvedExpression Annotation)
resolveStoreExpressionHandler prj se = do
  typeMap <- handleEither InternalError (getTypeMap prj)
  handleEither UserError $ resolveStoreExpression (prjStore prj) typeMap "" se

findExprHandler ::
  Project Annotation ->
  ExprHash ->
  Handler (StoreExpression Annotation)
findExprHandler project exprHash' =
  handleEither InternalError $
    case lookupExprHash project exprHash' of
      Nothing -> Left ("Could not find exprhash!" :: Text)
      Just a -> Right a

createNewUnitTestsHandler ::
  Project Annotation ->
  ExprHash ->
  ExprHash ->
  Handler
    ( Project Annotation,
      [StoreExpression Annotation]
    )
createNewUnitTestsHandler project oldExprHash newExprHash =
  handleEither UserError (createNewUnitTests project oldExprHash newExprHash)

createUnitTestHandler ::
  Project Annotation ->
  StoreExpression Annotation ->
  TestName ->
  Handler UnitTest
createUnitTestHandler project storeExpr testName =
  handleEither UserError $ createUnitTest project storeExpr testName
