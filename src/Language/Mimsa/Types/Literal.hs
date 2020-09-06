{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Literal
  ( Literal (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.StringType

-------

data Literal
  = MyInt Int
  | MyBool Bool
  | MyString StringType
  | MyUnit
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      JSON.FromJSON,
      JSON.ToJSON
    )

instance Printer Literal where
  prettyDoc = renderLiteral

renderLiteral :: Literal -> Doc ann
renderLiteral (MyInt i) = pretty i
renderLiteral (MyBool True) = "True"
renderLiteral (MyBool False) = "False"
renderLiteral (MyString str) = dquotes $ prettyDoc str
renderLiteral MyUnit = "Unit"
