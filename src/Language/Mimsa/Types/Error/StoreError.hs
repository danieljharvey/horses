{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.StoreError where

import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

data StoreError
  = ExpressionDoesNotMatchHash ExprHash ExprHash
  | CouldNotReadFilePath FilePath
  | CouldNotWriteFilePath FilePath
  | CouldNotDecodeJson ExprHash
  | CouldNotDecodeFile FilePath
  | CouldNotFindExprHashForBindings [Name]
  | CouldNotFindExprHashForTypeBindings [TyCon]
  | CouldNotFindBinding Name
  | CouldNotFindStoreExpression ExprHash
  | UnknownStoreError
  deriving (Eq, Ord, Show)

instance Printer StoreError where
  prettyPrint (ExpressionDoesNotMatchHash a b) =
    "Expression hashes does not match expected: "
      <> prettyPrint a
      <> " !== "
      <> prettyPrint b
  prettyPrint (CouldNotReadFilePath path) =
    "Could not read file at path " <> T.pack path
  prettyPrint (CouldNotWriteFilePath path) =
    "Could not write file at path " <> T.pack path
  prettyPrint (CouldNotDecodeJson hash') =
    "Could not decode JSON for hash " <> prettyPrint hash'
  prettyPrint (CouldNotDecodeFile path) =
    "Could not decode JSON for file " <> T.pack path
  prettyPrint (CouldNotFindExprHashForBindings missing) =
    "Could not find expressions in the store for the following: "
      <> T.intercalate "," (prettyPrint <$> missing)
  prettyPrint (CouldNotFindExprHashForTypeBindings missing) =
    "Could not find type expressions in the store for the following: "
      <> T.intercalate "," (prettyPrint <$> missing)
  prettyPrint (CouldNotFindBinding name) =
    "Could not find binding " <> prettyPrint name
  prettyPrint (CouldNotFindStoreExpression exprHash) =
    "Could not find store expression for hash " <> prettyPrint exprHash
  prettyPrint UnknownStoreError =
    "Unknown store error"

instance Semigroup StoreError where
  _ <> b = b

instance Monoid StoreError where
  mempty = UnknownStoreError
