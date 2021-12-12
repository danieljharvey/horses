{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.AddUnitTest
  ( addUnitTestHandler,
    AddUnitTest,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Helpers.ExpressionData
import Language.Mimsa.Server.MimsaHandler
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Language.Mimsa.UnitTests.Types
import Servant

------

type AddUnitTest =
  "tests" :> "add"
    :> ReqBody '[JSON] AddUnitTestRequest
    :> JsonPost AddUnitTestResponse

data AddUnitTestRequest = AddUnitTestRequest
  { autProjectHash :: ProjectHash,
    autTestName :: TestName,
    autExpression :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data AddUnitTestResponse = AddUnitTestResponse
  { autProjectData :: ProjectData,
    autUnitTest :: UnitTestData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

addUnitTestHandler ::
  MimsaEnvironment ->
  AddUnitTestRequest ->
  MimsaHandler AddUnitTestResponse
addUnitTestHandler mimsaEnv (AddUnitTestRequest hash testName testInput) = runMimsaHandlerT $ do
  let action = do
        expr <- Actions.parseExpr testInput
        Actions.addUnitTest expr testName testInput
  response <-
    lift $ eitherFromActionM mimsaEnv hash action
  case response of
    Right (newProject, unitTest) -> do
      pd <- lift $ projectDataHandler mimsaEnv newProject
      returnMimsa $
        AddUnitTestResponse
          pd
          (mkUnitTestData newProject unitTest)
    Left e -> throwMimsaError e
