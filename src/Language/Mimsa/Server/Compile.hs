{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Compile (CompileAPI, compileEndpoints) where

import qualified Data.ByteString.Lazy as LBS
import Data.Swagger (NamedSchema (..), ToSchema, binarySchema, declareNamedSchema)
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Store
import Servant

-----

type CompileAPI =
  "compile"
    :> CompileExpression

compileEndpoints ::
  MimsaEnvironment ->
  Server CompileAPI
compileEndpoints =
  compileExpressionEndpoint

newtype CompileExpressionResponse = CompileExpressionResponse LBS.ByteString
  deriving newtype (MimeRender OctetStream)

instance ToSchema CompileExpressionResponse where
  declareNamedSchema _cer = pure (NamedSchema Nothing binarySchema)

------

-- return type of a ZIP file download with a filename in it's header
type CompileResponse =
  ( Headers
      '[Header "Content-Disposition" String]
      CompileExpressionResponse
  )

type CompileExpression =
  "expression"
    :> Capture "exprHash" ExprHash
    :> Get
         '[OctetStream]
         CompileResponse

compileExpressionEndpoint ::
  MimsaEnvironment ->
  ExprHash ->
  Handler CompileResponse
compileExpressionEndpoint _mimsaEnv exprHash = do
  let filename = "mimsa-" <> show exprHash <> ".zip"
      contentDisposition = "attachment; filename=\"" <> filename <> "\""
  pure (addHeader contentDisposition (CompileExpressionResponse "poo"))
