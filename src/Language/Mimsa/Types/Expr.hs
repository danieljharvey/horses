{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Expr
  ( Expr (..),
  )
where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.DataType
import Language.Mimsa.Types.Literal
import Language.Mimsa.Types.Name

-------

data Expr a
  = MyLiteral Literal
  | MyVar a
  | MyLet a (Expr a) (Expr a) -- binder, expr, body
  | MyLetPair a a (Expr a) (Expr a) -- binderA, binderB, expr, body
  | MyLambda a (Expr a) -- binder, body
  | MyApp (Expr a) (Expr a) -- function, argument
  | MyIf (Expr a) (Expr a) (Expr a) -- expr, thencase, elsecase
  | MyPair (Expr a) (Expr a) -- (a,b)
  | MyRecord (Map Name (Expr a)) -- { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
  | MyRecordAccess (Expr a) Name -- a.foo
  | MyData DataType (Expr a) -- tyName, tyArgs, Map constructor args, body
  | MyConstructor Construct -- use a constructor by name
  | MyConsApp (Expr a) (Expr a) -- constructor, value
  | MyCaseMatch (Expr a) (NonEmpty (Construct, Expr a)) (Maybe (Expr a)) -- expr, matches, catchAll
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance (Show a, Printer a) => Printer (Expr a) where
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
  prettyPrint (MyRecord map') = "{" <> T.intercalate ", " exprs' <> "}"
    where
      exprs' =
        ( \(name, val) ->
            prettyPrint name
              <> ": "
              <> printSubExpr val
        )
          <$> M.toList map'
  prettyPrint (MyData dataType expr) =
    prettyPrint dataType
      <> " in "
      <> printSubExpr expr
  prettyPrint (MyConstructor name) = prettyPrint name
  prettyPrint (MyConsApp fn val) = prettyPrint fn <> " " <> printSubExpr val
  prettyPrint (MyCaseMatch sumExpr matches catchAll) =
    "case "
      <> printSubExpr sumExpr
      <> " of "
      <> T.intercalate " | " (printMatch <$> NE.toList matches)
      <> maybe "" (\catchExpr -> " | otherwise " <> printSubExpr catchExpr) catchAll
    where
      printMatch (construct, expr') =
        prettyPrint construct <> " " <> printSubExpr expr'

inParens :: (Show a, Printer a) => a -> Text
inParens a = "(" <> prettyPrint a <> ")"

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: (Show a, Printer a) => Expr a -> Text
printSubExpr expr = case expr of
  all'@MyLet {} -> inParens all'
  all'@MyLambda {} -> inParens all'
  all'@MyRecord {} -> inParens all'
  all'@MyIf {} -> inParens all'
  all'@MyConstructor {} -> inParens all'
  all'@MyConsApp {} -> inParens all'
  all'@MyPair {} -> inParens all'
  a -> prettyPrint a
