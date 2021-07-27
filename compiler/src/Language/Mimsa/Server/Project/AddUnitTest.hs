{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.AddUnitTest
  ( addUnitTestHandler,
    AddUnitTest,
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import Language.Mimsa.Server.ExpressionData
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Servant

------

type AddUnitTest =
  "tests" :> "add"
    :> ReqBody '[JSON] AddUnitTestRequest
    :> Post '[JSON] AddUnitTestResponse

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
  Handler AddUnitTestResponse
addUnitTestHandler mimsaEnv (AddUnitTestRequest hash testName testInput) = do
  expr <- parseHandler testInput
  (newProject, unitTest) <-
    fromActionM
      mimsaEnv
      hash
      (Actions.addUnitTest expr testName testInput)
  pd <- projectDataHandler mimsaEnv newProject
  pure $
    AddUnitTestResponse
      pd
      (mkUnitTestData newProject unitTest)
