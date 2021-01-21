{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Language.Mimsa.Project
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
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
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data AddUnitTestResponse = AddUnitTestResponse
  { autProjectData :: ProjectData,
    autUnitTest :: UnitTest
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

addUnitTestHandler ::
  MimsaEnvironment ->
  AddUnitTestRequest ->
  Handler AddUnitTestResponse
addUnitTestHandler mimsaEnv (AddUnitTestRequest hash testName testInput) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' hash
  (ResolvedExpression _ storeExpr _ _ _) <-
    evaluateTextHandler project testInput
  _ <- saveExprHandler mimsaEnv storeExpr
  test <- createUnitTestHandler project storeExpr testName
  let projectWithTest = fromUnitTest test storeExpr <> project
  writeStoreHandler mimsaEnv (prjStore projectWithTest)
  pd <- projectDataHandler mimsaEnv projectWithTest
  pure $
    AddUnitTestResponse
      pd
      test
