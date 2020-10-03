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
import GHC.Generics (Generic)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType (DataType)
import Language.Mimsa.Types.AST.Literal (Literal)
import Language.Mimsa.Types.Identifiers (Name, TyCon)

-------

data Expr a var
  = MyLiteral a Literal
  | MyVar a var
  | MyLet a var (Expr a var) (Expr a var) -- binder, expr, body
  | MyLetPair a var var (Expr a var) (Expr a var) -- binderA, binderB, expr, body
  | MyLambda a var (Expr a var) -- binder, body
  | MyApp a (Expr a var) (Expr a var) -- function, argument
  | MyIf a (Expr a var) (Expr a var) (Expr a var) -- expr, thencase, elsecase
  | MyPair a (Expr a var) (Expr a var) -- (a,b)
  | MyRecord a (Map Name (Expr a var)) -- { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
  | MyRecordAccess a (Expr a var) Name -- a.foo
  | MyData a DataType (Expr a var) -- tyName, tyArgs, Map constructor args, body
  | MyConstructor a TyCon -- use a constructor by name
  | MyConsApp a (Expr a var) (Expr a var) -- constructor, value
  | MyCaseMatch a (Expr a var) (NonEmpty (TyCon, Expr a var)) (Maybe (Expr a var)) -- expr, matches, catchAll
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance (Show var, Printer var) => Printer (Expr a var) where
  prettyDoc (MyLiteral _ l) = prettyDoc l
  prettyDoc (MyVar _ var) = prettyDoc var
  prettyDoc (MyLet _ var expr1 expr2) =
    "let" <+> prettyDoc var
      <+> "="
      <+> prettyDoc expr1
      <> ";"
      <> line
      <> prettyDoc expr2
  prettyDoc (MyLetPair _ var1 var2 expr1 body) =
    "let" <+> "(" <+> prettyDoc var1 <+> "," <+> prettyDoc var2
      <+> ")"
      <+> "="
      <+> printSubExpr expr1
      <+> "in"
      <+> printSubExpr body
  prettyDoc (MyLambda _ binder expr) =
    vsep
      [ "\\"
          <> prettyDoc binder
          <+> "->",
        indent 3 $
          prettyDoc expr
      ]
  prettyDoc (MyApp _ func arg) =
    printSubExpr func <> parens (printSubExpr arg)
  prettyDoc (MyRecordAccess _ expr name) =
    printSubExpr expr <> "." <> prettyDoc name
  prettyDoc (MyIf _ if' then' else') =
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
  prettyDoc (MyPair _ a b) =
    tupled
      [ printSubExpr a,
        printSubExpr b
      ]
  prettyDoc (MyRecord _ map') = encloseSep lbrace rbrace comma exprs'
    where
      exprs' =
        ( \(name, val) ->
            prettyDoc name
              <> ": "
              <> printSubExpr val
        )
          <$> M.toList map'
  prettyDoc (MyData _ dataType expr) =
    prettyDoc dataType
      <> ";"
      <> line
      <> prettyDoc expr
  prettyDoc (MyConstructor _ name) = prettyDoc name
  prettyDoc (MyConsApp _ fn val) = prettyDoc fn <+> printSubExpr val
  prettyDoc (MyCaseMatch _ sumExpr matches catchAll) =
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

inParens :: (Show var, Printer var) => Expr a var -> Doc ann
inParens = parens . prettyDoc

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: (Show var, Printer var) => Expr a var -> Doc ann
printSubExpr expr = case expr of
  all'@MyLet {} -> inParens all'
  all'@MyLambda {} -> inParens all'
  all'@MyRecord {} -> inParens all'
  all'@MyIf {} -> inParens all'
  all'@MyConstructor {} -> inParens all'
  all'@MyConsApp {} -> inParens all'
  all'@MyPair {} -> inParens all'
  a -> prettyDoc a
