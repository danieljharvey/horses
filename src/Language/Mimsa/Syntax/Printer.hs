{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Syntax.Printer
  ( Printer (..),
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
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
  prettyPrint (MyLetList var1 var2 expr body) =
    "let [" <> prettyPrint var1 <> ", " <> prettyPrint var2
      <> "] = "
      <> printSubExpr expr
      <> " in "
      <> printSubExpr body
  prettyPrint (MyLambda binder expr) =
    "\\"
      <> prettyPrint binder
      <> " -> "
      <> printSubExpr expr
  prettyPrint (MyApp (MyApp (MyApp func arg1) arg2) arg3) =
    printSubExpr func <> "("
      <> printSubExpr arg1
      <> ")("
      <> printSubExpr arg2
      <> ")("
      <> printSubExpr arg3
      <> ")"
  prettyPrint (MyApp (MyApp func arg1) arg2) =
    printSubExpr func <> "("
      <> printSubExpr arg1
      <> ")("
      <> printSubExpr arg2
      <> ")"
  prettyPrint (MyApp func arg) =
    printSubExpr func <> "("
      <> printSubExpr arg
      <> ")"
  prettyPrint (MyRecordAccess expr name) =
    printSubExpr expr <> "." <> prettyPrint name
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
  prettyPrint (MySum MyLeft a) = "Left " <> printSubExpr a
  prettyPrint (MySum MyRight b) = "Right " <> printSubExpr b
  prettyPrint (MyCase sumExpr leftFunc rightFunc) =
    "case "
      <> printSubExpr sumExpr
      <> " of Left "
      <> printSubExpr leftFunc
      <> " | Right "
      <> printSubExpr rightFunc
  prettyPrint (MyList as) = "[" <> T.intercalate ", " exprs' <> "]"
    where
      exprs' = NE.toList $ printSubExpr <$> as
  prettyPrint (MyRecord map') = "{" <> T.intercalate ", " exprs' <> "}"
    where
      exprs' =
        ( \(name, val) ->
            prettyPrint name
              <> ": "
              <> printSubExpr val
        )
          <$> (M.toList map')

inParens :: (Printer a) => a -> Text
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
  prettyPrint (MTFunction a b) = printSubType a <> " -> " <> printSubType b
  prettyPrint (MTPair a b) = "(" <> printSubType a <> ", " <> printSubType b <> ")"
  prettyPrint (MTVar a) = prettyPrint a
  prettyPrint (MTSum a b) = "Sum " <> printSubType a <> " " <> printSubType b
  prettyPrint (MTList a) = "List " <> printSubType a
  prettyPrint (MTRecord as) = "{" <> T.intercalate ", " types <> "}"
    where
      types =
        ( \(name, mt) ->
            prettyPrint name
              <> ": "
              <> printSubType mt
        )
          <$> (M.toList as)

-- simple things with no brackets, complex things in brackets
printSubType :: MonoType -> Text
printSubType all'@(MTSum _ _) = inParens all'
printSubType all'@(MTFunction _ _) = inParens all'
printSubType all'@(MTList _) = inParens all'
printSubType a = prettyPrint a
