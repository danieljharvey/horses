{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Search (SearchAPI, searchEndpoints) where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.OpenApi hiding (Server)
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Types.Project
import Servant
import Server.Handlers
import Server.Helpers
import Server.Types

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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

newtype TypeSearchResponse = TypeSearchResponse
  { tsProjectMatches :: [Text]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

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
  (_, _, typeMap) <- handleEither InternalError (Actions.run project Actions.typeMapForProjectSearch)
  result <- handleEither UserError (typeSearchFromText typeMap input)
  pure (TypeSearchResponse (prettyPrint <$> M.keys result))
