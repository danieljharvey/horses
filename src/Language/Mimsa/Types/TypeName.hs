{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeName where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Name

data TypeName = ConsName Construct [TypeName] | VarName Name
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON)

instance Printer TypeName where
  prettyPrint (ConsName c []) = prettyPrint c
  prettyPrint (ConsName c tys) = "(" <> prettyPrint c <> vars <> ")"
    where
      vars =
        " " <> T.intercalate " " (prettyPrint <$> tys)
  prettyPrint (VarName v) = prettyPrint v
