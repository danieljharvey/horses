{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Usage where

import qualified Data.Text as T
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer

data Usage
  = Transient Name ExprHash
  | Direct Name ExprHash
  deriving (Eq, Ord, Show)

instance Printer Usage where
  prettyPrint (Transient name _) =
    "Transient dependency of "
      <> prettyPrint name
  prettyPrint (Direct name _) =
    "Direct dependency of "
      <> prettyPrint name

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
