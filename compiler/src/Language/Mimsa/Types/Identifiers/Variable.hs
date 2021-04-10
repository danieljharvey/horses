{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.Variable
  ( Variable (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name

data Variable
  = NamedVar Name
  | NumberedVar Int
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.ToJSONKey)

instance Printer Variable where
  prettyDoc = renderVariable

renderVariable :: Variable -> Doc ann
renderVariable (NamedVar n) = renderName n
renderVariable (NumberedVar i) = "U" <> pretty i
