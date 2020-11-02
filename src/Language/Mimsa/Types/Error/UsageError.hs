{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.UsageError where

import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ExprHash

----------

data UsageError
  = CouldNotResolveDeps [Name]
  | CouldNotFindBinding Name
  | CouldNotFindStoreExpression ExprHash
  deriving (Eq, Ord, Show)

instance Printer UsageError where
  prettyPrint (CouldNotResolveDeps names) =
    "Could not resolve deps: " <> T.intercalate ", " (prettyPrint <$> names)
  prettyPrint (CouldNotFindBinding name) =
    "Could not find binding " <> prettyPrint name
  prettyPrint (CouldNotFindStoreExpression exprHash) =
    "Could not find store expression for hash " <> prettyPrint exprHash
