{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Types.AST.Literal
  ( Literal (..),
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Core.Printer
import Language.Mimsa.Core.Types.AST.StringType
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
      JSON.ToJSON
    )

instance Printer Literal where
  prettyDoc = renderLiteral

renderLiteral :: Literal -> Doc ann
renderLiteral (MyInt i) = pretty i
renderLiteral (MyBool True) = "True"
renderLiteral (MyBool False) = "False"
renderLiteral (MyString str) = dquotes $ prettyDoc str
