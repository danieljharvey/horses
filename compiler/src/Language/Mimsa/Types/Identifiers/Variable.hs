{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.Variable
  ( Variable (..),
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi (ToSchema)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name
import Prettyprinter

data Variable
  = NamedVar Name
  | NumberedVar Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.ToJSONKey, JSON.FromJSON, ToSchema)

instance Printer Variable where
  prettyDoc = renderVariable

printTypeNum :: Int -> String
printTypeNum i = [toEnum (index + start)] <> suffix
  where
    index = (i - 1) `mod` 26
    start = fromEnum 'a'
    suffix =
      let diff = (i - 1) `div` 26
       in if diff < 1 then "" else show diff

renderVariable :: Variable -> Doc ann
renderVariable (NamedVar n) = renderName n
renderVariable (NumberedVar i) = pretty (printTypeNum i)
