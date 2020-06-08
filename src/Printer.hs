{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Data.Text (Text)
import qualified Data.Text as T
import Types

class Printer a where

  prettyPrint :: a -> Text

  default prettyPrint :: (Show a) => a -> Text
  prettyPrint = T.pack . show

instance Printer Name where
  prettyPrint = getName

instance Printer Expr where
  prettyPrint (MyInt i) = T.pack (show i)
  prettyPrint (MyBool True) = "True"
  prettyPrint (MyBool False) = "False"
  prettyPrint (MyString str) = "\"" <> str <> "\""
  prettyPrint (MyVar var) = prettyPrint var
  prettyPrint (MyLet var expr1 expr2) =
    "let " <> prettyPrint var
      <> " = "
      <> prettyPrint expr1
      <> " in "
      <> prettyPrint expr2
  prettyPrint (MyLambda binder expr) =
    "\\"
      <> prettyPrint binder
      <> " -> "
      <> prettyPrint expr
  prettyPrint (MyApp func arg) = prettyPrint func <> " " <> prettyPrint arg
  prettyPrint (MyIf if' then' else') =
    "if "
      <> prettyPrint if'
      <> " then "
      <> prettyPrint then'
      <> " else "
      <> prettyPrint else'
