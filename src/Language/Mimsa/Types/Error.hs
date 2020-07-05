{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error where

import Data.Text (Text)
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.ResolverError
import Language.Mimsa.Types.TypeError

data Error
  = TypeErr TypeError
  | ResolverErr ResolverError
  | OtherError Text
  deriving (Eq, Ord, Show)

instance Printer Error where
  prettyPrint (TypeErr t) = "TypeError: " <> prettyPrint t
  prettyPrint (ResolverErr a) = "ResolverError: " <> prettyPrint a
  prettyPrint (OtherError a) = "OtherError: " <> a
