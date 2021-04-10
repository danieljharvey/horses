{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Search (SearchAPI, searchEndpoints) where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Helpers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Servant

-----

type SearchAPI =
  "search"
    :> TypeSearch

searchEndpoints ::
  MimsaEnvironment ->
  Server SearchAPI
searchEndpoints =
  typeSearchEndpoint

------

data TypeSearchRequest = TypeSearchRequest
  { tsProjectHash :: ProjectHash,
    tsInput :: Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype TypeSearchResponse = TypeSearchResponse
  { tsProjectMatches :: [Text]
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

type TypeSearch =
  "type"
    :> ReqBody '[JSON] TypeSearchRequest
    :> Post '[JSON] TypeSearchResponse

typeSearchEndpoint ::
  MimsaEnvironment ->
  TypeSearchRequest ->
  Handler TypeSearchResponse
typeSearchEndpoint mimsaEnv (TypeSearchRequest projectHash input) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  typeMap <- handleEither InternalError (getTypeMap project)
  result <- handleEither UserError (typeSearchFromText typeMap input)
  pure (TypeSearchResponse (prettyPrint <$> M.keys result))
