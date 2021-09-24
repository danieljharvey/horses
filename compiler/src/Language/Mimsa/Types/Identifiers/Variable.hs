{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.Variable
  ( Variable (..),
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name
import Prettyprinter

data Variable
  = NamedVar Name
  | NumberedVar Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.ToJSONKey)

instance Printer Variable where
  prettyDoc = renderVariable

renderVariable :: Variable -> Doc ann
renderVariable (NamedVar n) = renderName n
renderVariable (NumberedVar i) = "U" <> pretty i
