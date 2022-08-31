{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Project
  ( projectEndpoints,
    ProjectAPI,
  )
where

import Servant
import Server.Endpoints.Project.AddUnitTest
import Server.Endpoints.Project.BindExpression
import Server.Endpoints.Project.BindType
import Server.Endpoints.Project.CreateProject
import Server.Endpoints.Project.Evaluate
import Server.Endpoints.Project.ListBindings
import Server.Endpoints.Project.ListTests
import Server.Endpoints.Project.Optimise
import Server.Endpoints.Project.Upgrade
import Server.Types

-----

-- the Project endpoints output data in a way that front ends would be
-- interested

-- exprHashes should be strings to stop JS getting lost

type ProjectAPI =
  "project"
    :> ( EvaluateAPI
           :<|> ListBindings
           :<|> CreateProject
           :<|> BindExpression
           :<|> BindType
           :<|> AddUnitTest
           :<|> ListTests
           :<|> ListTestsByExprHash
           :<|> UpgradeAPI
           :<|> OptimiseAPI
       )

projectEndpoints ::
  MimsaEnvironment ->
  Server ProjectAPI
projectEndpoints mimsaEnv =
  evaluate mimsaEnv
    :<|> listBindings mimsaEnv
    :<|> createProject mimsaEnv
    :<|> bindExpression mimsaEnv
    :<|> bindType mimsaEnv
    :<|> addUnitTestHandler mimsaEnv
    :<|> listTestsHandler mimsaEnv
    :<|> listTestsByExprHashHandler mimsaEnv
    :<|> upgrade mimsaEnv
    :<|> optimise mimsaEnv

------
