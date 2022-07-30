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
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Types

-----

type CompileAPI =
  "compile"
    :> CompileHash

compileEndpoints ::
  MimsaEnvironment ->
  Server CompileAPI
compileEndpoints =
  compileHashEndpoint

-----

newtype ZipFileResponse = ZipFileResponse LBS.ByteString
  deriving newtype (MimeRender OctetStream)

instance ToSchema ZipFileResponse where
  declareNamedSchema _cer = pure (NamedSchema Nothing binarySchema)

-----

data CompileHashRequest = CompileHashRequest
  { chExprHash :: ExprHash,
    chBackend :: Backend
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

-- return type of a ZIP file download with a filename in it's header
type CompileHashResponse =
  ( Headers
      '[Header "Content-Disposition" String]
      ZipFileResponse
  )

type CompileHash =
  "hash"
    :> ReqBody '[JSON] CompileHashRequest
    :> Post
         '[OctetStream]
         CompileHashResponse

compileHashEndpoint ::
  MimsaEnvironment ->
  CompileHashRequest ->
  Handler CompileHashResponse
compileHashEndpoint
  mimsaEnv
  (CompileHashRequest exprHash backend) = do
    (storeExpr, pd, _) <- projectFromExpressionHandler mimsaEnv exprHash
    (_, outcomes, (rootExprHash, _)) <-
      fromActionM mimsaEnv (pdHash pd) (Actions.compileStoreExpression backend storeExpr)
    let makeZip = encodeZipFile . zipFromSavedFiles . Actions.writeFilesFromOutcomes
    let filename = "mimsa-" <> show rootExprHash <> ".zip"
        contentDisposition = "attachment; filename=\"" <> filename <> "\""
    pure (addHeader contentDisposition (ZipFileResponse (makeZip outcomes)))
