{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Literal
  ( Literal (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.StringType

-------

-- | A literal value in the source code
data Literal
  = -- | an integer
    MyInt Int
  | -- | a boolean
    MyBool Bool
  | -- | a string
    MyString StringType
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
