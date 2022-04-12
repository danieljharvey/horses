{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Compile (CompileAPI, compileEndpoints) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.OpenApi (NamedSchema (..), ToSchema, binarySchema, declareNamedSchema)
import Data.Set (Set)
import GHC.Generics
import qualified Language.Mimsa.Actions.Compile as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Backend.ZipFile
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers
import Server.ServerConfig
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
    (_, (rootExprHash, exprHashes)) <-
      fromActionM mimsaEnv (pdHash pd) (Actions.compile backend storeExpr)
    let filename = "mimsa-" <> show rootExprHash <> ".zip"
        contentDisposition = "attachment; filename=\"" <> filename <> "\""
    bs <- doCreateZipFile mimsaEnv backend exprHashes rootExprHash
    pure (addHeader contentDisposition (ZipFileResponse bs))

-----

doCreateZipFile ::
  MimsaEnvironment ->
  Backend ->
  Set ExprHash ->
  ExprHash ->
  Handler LBS.ByteString
doCreateZipFile mimsaEnv be exprHashes rootExprHash = do
  let mimsaCfg = mimsaConfig mimsaEnv
  archive <-
    handleServerM @()
      mimsaCfg
      InternalError
      ( createZipFile (scRootPath mimsaCfg) be exprHashes rootExprHash
      )
  pure (encodeZipFile archive)
