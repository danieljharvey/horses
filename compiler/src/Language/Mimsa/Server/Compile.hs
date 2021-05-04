{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Compile (CompileAPI, compileEndpoints) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import qualified Data.Map as M
import Data.Set (Set)
import Data.Swagger (NamedSchema (..), ToSchema, binarySchema, declareNamedSchema)
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Compile as Actions
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.ZipFile
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Helpers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Servant

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

data CompileExpressionRequest = CompileExpressionRequest
  { ceProjectHash :: ProjectHash,
    ceExpression :: Text,
    ceRuntime :: RuntimeName
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype ZipFileResponse = ZipFileResponse LBS.ByteString
  deriving newtype (MimeRender OctetStream)

instance ToSchema ZipFileResponse where
  declareNamedSchema _cer = pure (NamedSchema Nothing binarySchema)

-----

newtype CompileHashRequest = CompileHashRequest
  { chExprHash :: ExprHash
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
  (CompileHashRequest exprHash) = do
    store <- storeFromExprHashHandler mimsaEnv exprHash
    let project = fromStore store $> mempty
    storeExpr <- findExprHandler project exprHash
    pd <- projectDataHandler mimsaEnv project
    let input = prettyPrint (storeExpression storeExpr)
    runtime <- getRuntime (RuntimeName "export")
    (_, (rootExprHash, exprHashes)) <-
      fromActionM mimsaEnv (pdHash pd) (Actions.compile runtime input storeExpr)
    let filename = "mimsa-" <> show rootExprHash <> ".zip"
        contentDisposition = "attachment; filename=\"" <> filename <> "\""
    bs <- doCreateZipFile mimsaEnv runtime exprHashes rootExprHash
    pure (addHeader contentDisposition (ZipFileResponse bs))

-----

getRuntime :: RuntimeName -> Handler (Runtime Javascript)
getRuntime runtimeName =
  handleEither
    InternalError
    ( case M.lookup runtimeName runtimes of
        Just rt -> pure rt
        _ -> throwError ("Could not find runtime" :: Text)
    )

doCreateZipFile ::
  MimsaEnvironment ->
  Runtime code ->
  Set ExprHash ->
  ExprHash ->
  Handler LBS.ByteString
doCreateZipFile mimsaEnv runtime exprHashes rootExprHash = do
  let mimsaCfg = mimsaConfig mimsaEnv
  archive <-
    handleMimsaM
      mimsaCfg
      InternalError
      ( createZipFile runtime exprHashes rootExprHash
      )
  pure (encodeZipFile archive)
