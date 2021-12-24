{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project
  ( projectEndpoints,
    ProjectAPI,
  )
where

import Language.Mimsa.Server.Project.AddUnitTest
import Language.Mimsa.Server.Project.BindExpression
import Language.Mimsa.Server.Project.BindType
import Language.Mimsa.Server.Project.CreateProject
import Language.Mimsa.Server.Project.Evaluate
import Language.Mimsa.Server.Project.GetExpression
import Language.Mimsa.Server.Project.Graph
import Language.Mimsa.Server.Project.ListBindings
import Language.Mimsa.Server.Project.ListTests
import Language.Mimsa.Server.Types
import Servant

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
