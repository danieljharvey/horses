{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeName where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer

data TypeName = ConsName Construct [TypeName] | VarName Name
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON)

instance Printer TypeName where
  prettyPrint (ConsName c tys) = prettyPrint c <> vars tys
    where
      vars ty = case ty of
        [] -> ""
        as -> T.intercalate " " (prettyPrint <$> as)
  prettyPrint (VarName v) = prettyPrint v
