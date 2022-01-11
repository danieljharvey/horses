{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Literal
  ( Literal (..),
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.StringType
import Prettyprinter

-------

-- | A literal value in the source code
data Literal
  = -- | an integer
    MyInt {litInt :: Int}
  | -- | a boolean
    MyBool {litBool :: Bool}
  | -- | a string
    MyString {litString :: StringType}
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving anyclass
    ( JSON.FromJSON,
      JSON.ToJSON,
      ToSchema
    )

instance Printer Literal where
  prettyDoc = renderLiteral

renderLiteral :: Literal -> Doc ann
renderLiteral (MyInt i) = pretty i
renderLiteral (MyBool True) = "True"
renderLiteral (MyBool False) = "False"
renderLiteral (MyString str) = dquotes $ prettyDoc str
