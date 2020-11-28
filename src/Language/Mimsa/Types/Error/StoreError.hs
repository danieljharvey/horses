{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.StoreError where

import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Store

data StoreError
  = ExpressionDoesNotMatchHash ExprHash ExprHash
  | CouldNotReadFilePath FilePath
  | CouldNotDecodeJson ExprHash
  | CouldNotDecodeFile FilePath
  | NoRemoteServersToTry
  | UnknownStoreError
  deriving (Eq, Ord, Show)

instance Printer StoreError where
  prettyPrint (ExpressionDoesNotMatchHash a b) =
    "Expression hashes does not match expected: " <> prettyPrint a <> " !== " <> prettyPrint b
  prettyPrint (CouldNotReadFilePath path) = "Could not read file at path " <> T.pack path
  prettyPrint (CouldNotDecodeJson hash') = "Could not decode JSON for hash " <> prettyPrint hash'
  prettyPrint (CouldNotDecodeFile path) = "Could not decode JSON for file " <> T.pack path
  prettyPrint NoRemoteServersToTry = "No remote servers to fetch from"
  prettyPrint UnknownStoreError = "Unknown store error"

instance Semigroup StoreError where
  _ <> b = b

instance Monoid StoreError where
  mempty = UnknownStoreError
