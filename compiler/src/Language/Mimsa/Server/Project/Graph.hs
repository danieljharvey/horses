{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.Graph
  ( graphProject,
    GraphAPI,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi hiding (Server)
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Graph as Actions
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Store
import Language.Mimsa.Types.Project
import Servant

-- /project/graph/<projectHash>

type GraphAPI =
  "graph"
    :> ( Capture "projectHash" ProjectHash :> Get '[JSON] GraphProjectResponse
       )

newtype GraphProjectResponse = GraphProjectResponse {gpGraphviz :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

graphProject ::
  MimsaEnvironment -> Server GraphAPI
graphProject mimsaEnv projectHash = do
  (_newProject, gv) <-
    fromActionM
      mimsaEnv
      projectHash
      Actions.graphProject
  pure . GraphProjectResponse . prettyGraphviz $ gv
