{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.TypeName where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Name

data TypeName = ConsName Construct [TypeName] | VarName Name
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON)
