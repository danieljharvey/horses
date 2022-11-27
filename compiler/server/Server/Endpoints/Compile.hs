{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Compile (CompileAPI, compileEndpoints) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.OpenApi (NamedSchema (..), ToSchema, binarySchema, declareNamedSchema)
import GHC.Generics
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Backend.ZipFile
import Servant
import Server.Handlers
import Server.Helpers.ProjectData
import Server.Types
import Language.Mimsa.Types.Modules.ModuleHash

-----

type CompileAPI =
  "compile"
    :> CompileModule

compileEndpoints ::
  MimsaEnvironment ->
  Server CompileAPI
compileEndpoints =
  compileModuleEndpoint

-----

newtype ZipFileResponse = ZipFileResponse LBS.ByteString
  deriving newtype (MimeRender OctetStream)

instance ToSchema ZipFileResponse where
  declareNamedSchema _cer = pure (NamedSchema Nothing binarySchema)

-----

data CompileModuleRequest = CompileModuleRequest
  { chModuleHash :: ModuleHash,
    chBackend :: Backend
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

-- return type of a ZIP file download with a filename in it's header
type CompileModuleResponse =
  ( Headers
      '[Header "Content-Disposition" String]
      ZipFileResponse
  )

type CompileModule =
  "hash"
    :> ReqBody '[JSON] CompileModuleRequest
    :> Post
         '[OctetStream]
         CompileModuleResponse

compileModuleEndpoint ::
  MimsaEnvironment ->
  CompileModuleRequest ->
  Handler CompileModuleResponse
compileModuleEndpoint
  mimsaEnv
  (CompileModuleRequest moduleHash backend) = do
    (rootModule, pd, _) <- projectFromModuleHandler mimsaEnv moduleHash
    (_, outcomes, (rootModuleHash, _, _)) <-
      fromActionM mimsaEnv (pdHash pd) (Actions.compileModule backend rootModule)
    let makeZip = encodeZipFile . zipFromSavedFiles . Actions.writeFilesFromOutcomes
    let filename = "mimsa-" <> show rootModuleHash <> ".zip"
        contentDisposition = "attachment; filename=\"" <> filename <> "\""
    pure (addHeader contentDisposition (ZipFileResponse (makeZip outcomes)))
