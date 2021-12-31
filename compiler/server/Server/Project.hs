{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Project
  ( projectEndpoints,
    ProjectAPI,
  )
where

import Servant
import Server.Project.AddUnitTest
import Server.Project.BindExpression
import Server.Project.BindType
import Server.Project.CreateProject
import Server.Project.Evaluate
import Server.Project.GetExpression
import Server.Project.Graph
import Server.Project.ListBindings
import Server.Project.ListTests
import Server.Types

-----

-- the Project endpoints output data in a way that front ends would be
-- interested

-- exprHashes should be strings to stop JS getting lost

type ProjectAPI =
  "project"
    :> ( EvaluateAPI :<|> ListBindings :<|> GetExpression
           :<|> CreateProject
           :<|> BindExpression
           :<|> BindType
           :<|> AddUnitTest
           :<|> ListTests
           :<|> ListTestsByExprHash
           :<|> GraphAPI
       )

projectEndpoints ::
  MimsaEnvironment ->
  Server ProjectAPI
projectEndpoints mimsaEnv =
  evaluateExpression mimsaEnv
    :<|> listBindings mimsaEnv
    :<|> getExpression mimsaEnv
    :<|> createProject mimsaEnv
    :<|> bindExpression mimsaEnv
    :<|> bindType mimsaEnv
    :<|> addUnitTestHandler mimsaEnv
    :<|> listTestsHandler mimsaEnv
    :<|> listTestsByExprHashHandler mimsaEnv
    :<|> graphProject mimsaEnv

------
