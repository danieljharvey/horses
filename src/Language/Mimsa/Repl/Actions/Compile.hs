{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Compile
  ( doOutputJS,
  )
where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Language.Mimsa.Actions.Compile as Actions
import Language.Mimsa.Backend.Backend
  ( copyLocalOutput,
  )
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Monad
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

bsToText :: LBS.ByteString -> Text
bsToText = T.decodeUtf8 . B.concat . LB.toChunks

doOutputJS ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doOutputJS project input expr = do
  let runtime = exportRuntime
  (_, (rootExprHash, exprHashes)) <-
    toReplM project (Actions.compile runtime input expr)
  outputPath <- doCopying runtime exprHashes rootExprHash
  replOutput ("Output to " <> bsToText outputPath)

doCopying ::
  Runtime code ->
  Set ExprHash ->
  ExprHash ->
  MimsaM (Error Annotation) LBS.ByteString
doCopying runtime exprHashes rootExprHash =
  mapError StoreErr (copyLocalOutput runtime exprHashes rootExprHash)
