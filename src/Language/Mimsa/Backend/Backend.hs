{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Backend
  ( outputCommonJS,
    getStdlib,
    copyLocalOutput,
    Backend (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Set (Set)
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Store.Storage (tryCopy)
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store

------

getStdlib :: Backend -> LBS.ByteString
getStdlib CommonJS = coerce commonJSStandardLibrary

-- given output type and list of expressions, copy everything to local
-- folder for output in repl
copyLocalOutput ::
  Backend ->
  Set ExprHash ->
  ExprHash ->
  MimsaM StoreError LBS.ByteString
copyLocalOutput be exprHashes rootExprHash = do
  modulePath <- createModuleOutputPath be
  stdlibPath <- createStdlibOutputPath be
  indexPath <- createIndexOutputPath be
  outputPath <- createOutputFolder be rootExprHash
  -- link modules
  traverse_ (copyModule modulePath outputPath be) exprHashes
  -- link stdlib
  _ <- copyStdlib stdlibPath outputPath be
  -- link index
  copyIndex indexPath outputPath be rootExprHash

copyModule :: FilePath -> FilePath -> Backend -> ExprHash -> MimsaM StoreError ()
copyModule modulePath outputPath be exprHash = do
  let filename = moduleFilename be exprHash
      fromPath = modulePath <> LB.unpack filename
      toPath = outputPath <> LB.unpack filename
  tryCopy fromPath toPath

-- the stdlib is already in the store so we copy it to the target folder
copyStdlib :: FilePath -> FilePath -> Backend -> MimsaM StoreError LBS.ByteString
copyStdlib stdlibPath outputPath be = do
  let fromPath = LB.pack stdlibPath <> stdLibFilename be
  let toPath = LB.pack outputPath <> stdLibFilename be
  tryCopy (LB.unpack fromPath) (LB.unpack toPath)
  pure toPath

-- the index is already in ths store so we copy it to the target folder
copyIndex ::
  FilePath ->
  FilePath ->
  Backend ->
  ExprHash ->
  MimsaM StoreError LBS.ByteString
copyIndex indexPath outputPath be rootExprHash = do
  let filename = LB.unpack $ indexFilename be rootExprHash
      fromPath = indexPath <> filename
      toPath = outputPath <> filename
  tryCopy fromPath toPath
  pure (LB.pack toPath)
