{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Syntax.Printer
  ( Printer (..),
  )
where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types

class Printer a where

  prettyPrint :: a -> Text

  default prettyPrint :: (Show a) => a -> Text
  prettyPrint = T.pack . show

instance (Printer a) => Printer (S.Set a) where
  prettyPrint as = foldr (\a as' -> prettyPrint a <> ", " <> as') "" as

instance Printer Name where
  prettyPrint = getName

instance Printer StringType where
  prettyPrint (StringType s) = s

instance Printer ExprHash where
  prettyPrint (ExprHash a) = T.pack . show $ a

instance Printer FuncName where
  prettyPrint (FuncName a) = a

instance Printer Literal where
  prettyPrint (MyInt i) = T.pack (show i)
  prettyPrint (MyBool True) = "True"
  prettyPrint (MyBool False) = "False"
  prettyPrint (MyString str) = "\"" <> prettyPrint str <> "\""
  prettyPrint MyUnit = "Unit"

instance Printer Expr where
  prettyPrint (MyLiteral l) = prettyPrint l
  prettyPrint (MyVar var) = prettyPrint var
  prettyPrint (MyLet var expr1 expr2) =
    "let " <> prettyPrint var
      <> " = "
      <> printSubExpr expr1
      <> " in "
      <> printSubExpr expr2
  prettyPrint (MyLetPair var1 var2 expr1 body) =
    "let (" <> prettyPrint var1 <> ", " <> prettyPrint var2
      <> ") = "
      <> printSubExpr expr1
      <> " in "
      <> printSubExpr body
  prettyPrint (MyLambda binder expr) =
    "\\"
      <> prettyPrint binder
      <> " -> "
      <> printSubExpr expr
  prettyPrint (MyApp func arg) =
    "(" <> printSubExpr func <> " "
      <> printSubExpr arg
      <> ")"
  prettyPrint (MyIf if' then' else') =
    "if "
      <> printSubExpr if'
      <> " then "
      <> printSubExpr then'
      <> " else "
      <> printSubExpr else'
  prettyPrint (MyPair a b) =
    "("
      <> printSubExpr a
      <> ", "
      <> printSubExpr b
      <> ")"

inParens :: Expr -> Text
inParens a = "(" <> prettyPrint a <> ")"

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: Expr -> Text
printSubExpr expr = case expr of
  all'@(MyLet _ _ _) -> inParens all'
  all'@(MyLambda _ _) -> inParens all'
  all'@(MyApp _ _) -> inParens all'
  all'@(MyIf _ _ _) -> inParens all'
  a -> prettyPrint a

-----------------

instance Printer UniVar where
  prettyPrint (UniVar a) = T.pack . show $ a

instance Printer MonoType where
  prettyPrint MTInt = "Int"
  prettyPrint MTString = "String"
  prettyPrint MTBool = "Boolean"
  prettyPrint MTUnit = "Unit"
  prettyPrint (MTFunction a b) = prettyPrint a <> " -> " <> prettyPrint b
  prettyPrint (MTPair a b) = "(" <> prettyPrint a <> ", " <> prettyPrint b <> ")"
  prettyPrint (MTVar a) = prettyPrint a
