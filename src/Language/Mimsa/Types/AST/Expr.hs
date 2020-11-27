{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Types.AST.Expr
  ( Expr (..),
    toEmptyAnnotation,
    getAnnotation,
    mapExpr,
    bindExpr,
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Swagger (ToSchema)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType (DataType)
import Language.Mimsa.Types.AST.Literal (Literal)
import Language.Mimsa.Types.AST.Operator
import Language.Mimsa.Types.Identifiers (Name, TyCon)

-------

data Expr var ann
  = MyLiteral ann Literal
  | MyVar ann var
  | MyLet ann var (Expr var ann) (Expr var ann) -- binder, expr, body
  | MyLetPair ann var var (Expr var ann) (Expr var ann) -- binderA, binderB, expr, body
  | MyInfix ann Operator (Expr var ann) (Expr var ann) -- a `f` b
  | MyLambda ann var (Expr var ann) -- binder, body
  | MyApp ann (Expr var ann) (Expr var ann) -- function, argument
  | MyIf ann (Expr var ann) (Expr var ann) (Expr var ann) -- expr, thencase, elsecase
  | MyPair ann (Expr var ann) (Expr var ann) -- (a,b)
  | MyRecord ann (Map Name (Expr var ann)) -- { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
  | MyRecordAccess ann (Expr var ann) Name -- a.foo
  | MyData ann DataType (Expr var ann) -- tyName, tyArgs, Map constructor args, body
  | MyConstructor ann TyCon -- use a constructor by name
  | MyConsApp ann (Expr var ann) (Expr var ann) -- constructor, value
  | MyCaseMatch ann (Expr var ann) (NonEmpty (TyCon, Expr var ann)) (Maybe (Expr var ann)) -- expr, matches, catchAll
  deriving (Eq, Ord, Show, Functor, Generic, JSON.FromJSON, JSON.ToJSON)

deriving instance (ToSchema var, ToSchema ann) => ToSchema (Expr var ann)

toEmptyAnnotation ::
  (Monoid b) =>
  Expr var a ->
  Expr var b
toEmptyAnnotation = fmap (const mempty)

getAnnotation :: Expr var ann -> ann
getAnnotation (MyLiteral ann _) = ann
getAnnotation (MyVar ann _) = ann
getAnnotation (MyLet ann _ _ _) = ann
getAnnotation (MyLetPair ann _ _ _ _) = ann
getAnnotation (MyInfix ann _ _ _) = ann
getAnnotation (MyLambda ann _ _) = ann
getAnnotation (MyApp ann _ _) = ann
getAnnotation (MyIf ann _ _ _) = ann
getAnnotation (MyPair ann _ _) = ann
getAnnotation (MyRecord ann _) = ann
getAnnotation (MyRecordAccess ann _ _) = ann
getAnnotation (MyData ann _ _) = ann
getAnnotation (MyConstructor ann _) = ann
getAnnotation (MyConsApp ann _ _) = ann
getAnnotation (MyCaseMatch ann _ _ _) = ann

mapExpr :: (Expr a b -> Expr a b) -> Expr a b -> Expr a b
mapExpr _ (MyLiteral ann a) = MyLiteral ann a
mapExpr _ (MyVar ann a) = MyVar ann a
mapExpr f (MyLet ann binder bindExpr' inExpr) =
  MyLet ann binder (f bindExpr') (f inExpr)
mapExpr f (MyLetPair ann binderA binderB bindExpr' inExpr) =
  MyLetPair ann binderA binderB (f bindExpr') (f inExpr)
mapExpr f (MyInfix ann op a b) = MyInfix ann op (f a) (f b)
mapExpr f (MyLambda ann binder expr) = MyLambda ann binder (f expr)
mapExpr f (MyApp ann func arg) = MyApp ann (f func) (f arg)
mapExpr f (MyIf ann matchExpr thenExpr elseExpr) =
  MyIf ann (f matchExpr) (f thenExpr) (f elseExpr)
mapExpr f (MyPair ann a b) = MyPair ann (f a) (f b)
mapExpr f (MyRecord ann items) = MyRecord ann (f <$> items)
mapExpr f (MyRecordAccess ann expr name) =
  MyRecordAccess ann (f expr) name
mapExpr f (MyData ann dt expr) = MyData ann dt (f expr)
mapExpr _ (MyConstructor ann cons) = MyConstructor ann cons
mapExpr f (MyConsApp ann func arg) =
  MyConsApp ann (f func) (f arg)
mapExpr f (MyCaseMatch ann matchExpr caseExprs catchExpr) =
  MyCaseMatch ann (f matchExpr) (second f <$> caseExprs) (f <$> catchExpr)

bindExpr ::
  (Applicative m) =>
  (Expr a b -> m (Expr a b)) ->
  Expr a b ->
  m (Expr a b)
bindExpr _ (MyLiteral ann a) =
  pure $ MyLiteral ann a
bindExpr _ (MyVar ann a) =
  pure $ MyVar ann a
bindExpr f (MyLet ann binder bindExpr' inExpr) =
  MyLet ann binder <$> f bindExpr' <*> f inExpr
bindExpr f (MyLetPair ann binderA binderB bindExpr' inExpr) =
  MyLetPair ann binderA binderB <$> f bindExpr' <*> f inExpr
bindExpr f (MyInfix ann op a b) =
  MyInfix ann op <$> f a <*> f b
bindExpr f (MyLambda ann binder expr) =
  MyLambda ann binder <$> f expr
bindExpr f (MyApp ann func arg) =
  MyApp ann <$> f func <*> f arg
bindExpr f (MyIf ann matchExpr thenExpr elseExpr) =
  MyIf ann <$> f matchExpr <*> f thenExpr <*> f elseExpr
bindExpr f (MyPair ann a b) =
  MyPair ann <$> f a <*> f b
bindExpr f (MyRecord ann items) =
  MyRecord ann <$> traverse f items
bindExpr f (MyRecordAccess ann expr name) =
  MyRecordAccess ann <$> f expr <*> pure name
bindExpr f (MyData ann dt expr) =
  MyData ann dt <$> f expr
bindExpr _ (MyConstructor ann cons) =
  pure $ MyConstructor ann cons
bindExpr f (MyConsApp ann func arg) =
  MyConsApp ann <$> f func <*> f arg
bindExpr f (MyCaseMatch ann matchExpr caseExprs catchExpr) =
  MyCaseMatch
    ann
    <$> f matchExpr
    <*> traverse traverseSecond caseExprs
    <*> traverse f catchExpr
  where
    traverseSecond (a, b) = (a,) <$> f b

instance (Show var, Printer var) => Printer (Expr var ann) where
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
  prettyDoc (MyInfix _ op a b) =
    sep
      [ printSubExpr a,
        prettyDoc op,
        printSubExpr b
      ]
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

inParens :: (Show var, Printer var) => Expr var ann -> Doc style
inParens = parens . prettyDoc

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: (Show var, Printer var) => Expr var ann -> Doc style
printSubExpr expr = case expr of
  all'@MyLet {} -> inParens all'
  all'@MyLambda {} -> inParens all'
  all'@MyRecord {} -> inParens all'
  all'@MyIf {} -> inParens all'
  all'@MyConstructor {} -> inParens all'
  all'@MyConsApp {} -> inParens all'
  all'@MyPair {} -> inParens all'
  a -> prettyDoc a
