{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.TypeName where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer

data TypeName = ConsName Construct | VarName Name
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON)

instance Printer TypeName where
  prettyPrint (ConsName c) = prettyPrint c
  prettyPrint (VarName v) = prettyPrint v
