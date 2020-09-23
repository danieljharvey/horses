{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Expr
  ( Expr (..),
  )
where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType
import Language.Mimsa.Types.AST.Literal
import Language.Mimsa.Types.Identifiers

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
  prettyPrint = renderWithWidth 20 . prettyDoc

  prettyDoc (MyLiteral l) = prettyDoc l
  prettyDoc (MyVar var) = prettyDoc var
  prettyDoc (MyLet var expr1 expr2) =
    "let" <+> prettyDoc var
      <+> "="
      <+> prettyDoc expr1
      <> ";"
      <> line
      <> prettyDoc expr2
  prettyDoc (MyLetPair var1 var2 expr1 body) =
    "let" <+> "(" <+> prettyDoc var1 <+> "," <+> prettyDoc var2
      <+> ")"
      <+> "="
      <+> printSubExpr expr1
      <+> "in"
      <+> printSubExpr body
  prettyDoc (MyLambda binder expr) =
    vsep
      [ "\\"
          <> prettyDoc binder
          <+> "->",
        indent 3 $
          prettyDoc expr
      ]
  prettyDoc (MyApp (MyApp (MyApp func arg1) arg2) arg3) =
    printSubExpr func <> "("
      <> printSubExpr arg1
      <> ")("
      <> printSubExpr arg2
      <> ")("
      <> printSubExpr arg3
      <> ")"
  prettyDoc (MyApp (MyApp func arg1) arg2) =
    printSubExpr func <> "("
      <> printSubExpr arg1
      <> ")("
      <> printSubExpr arg2
      <> ")"
  prettyDoc (MyApp func arg) =
    printSubExpr func <> "("
      <> printSubExpr arg
      <> ")"
  prettyDoc (MyRecordAccess expr name) =
    printSubExpr expr <> "." <> prettyDoc name
  prettyDoc (MyIf if' then' else') =
    vsep
      [ "if"
          <+> printSubExpr if',
        indent
          2
          ( "then"
              <+> printSubExpr then'
          ),
        indent
          2
          ( "else"
              <+> printSubExpr else'
          )
      ]
  prettyDoc (MyPair a b) =
    tupled
      [ printSubExpr a,
        printSubExpr b
      ]
  prettyDoc (MyRecord map') = encloseSep lbrace rbrace comma exprs'
    where
      exprs' =
        ( \(name, val) ->
            prettyDoc name
              <> ": "
              <> printSubExpr val
        )
          <$> M.toList map'
  prettyDoc (MyData dataType expr) =
    prettyDoc dataType
      <> ";"
      <> line
      <> prettyDoc expr
  prettyDoc (MyConstructor name) = prettyDoc name
  prettyDoc (MyConsApp fn val) = prettyDoc fn <+> printSubExpr val
  prettyDoc (MyCaseMatch sumExpr matches catchAll) =
    "case"
      <+> printSubExpr sumExpr
      <+> "of"
      <+> line
        <> indent
          2
          ( align $
              vsep
                ( zipWith
                    (<+>)
                    (" " : repeat "|")
                    options
                )
          )
    where
      catchAll' = case catchAll of
        Just catchExpr -> pure ("otherwise" <+> printSubExpr catchExpr)
        _ -> mempty
      options =
        (printMatch <$> NE.toList matches) <> catchAll'
      printMatch (construct, expr') =
        prettyDoc construct <+> printSubExpr expr'

inParens :: (Show a, Printer a) => a -> Doc ann
inParens = parens . prettyDoc

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: (Show a, Printer a) => Expr a -> Doc ann
printSubExpr expr = case expr of
  all'@MyLet {} -> inParens all'
  all'@MyLambda {} -> inParens all'
  all'@MyRecord {} -> inParens all'
  all'@MyIf {} -> inParens all'
  all'@MyConstructor {} -> inParens all'
  all'@MyConsApp {} -> inParens all'
  all'@MyPair {} -> inParens all'
  a -> prettyDoc a
