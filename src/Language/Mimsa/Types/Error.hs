{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error where

import Data.Text (Text)
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.TypeError

data Error
  = TypeErr TypeError
  | OtherError Text
  deriving (Eq, Ord, Show)

instance Printer Error where
  prettyPrint (TypeErr t) = "TypeError: " <> prettyPrint t
  prettyPrint (OtherError a) = "OtherError: " <> a
