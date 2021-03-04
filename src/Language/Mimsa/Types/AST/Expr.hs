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
    withMonoid,
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Swagger (ToSchema)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType (DataType)
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.AST.Literal (Literal)
import Language.Mimsa.Types.AST.Operator
import Language.Mimsa.Types.Identifiers (Name, TyCon)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as =
  uncurry f <$> zip [1 ..] as

-------

-- |
-- The main expression type that we parse from syntax
-- `var` is the type of variables. When we parse them they are
-- string-based `Name`, but after substitution they become a `Variable`
-- which is either a string or a numbered variable
data Expr var ann
  = -- | a literal, such as String, Int, Boolean
    MyLiteral ann Literal
  | -- | a named variable
    MyVar ann var
  | -- | binder, expr, body
    MyLet ann var (Expr var ann) (Expr var ann)
  | -- | binderA, binderB, expr, body
    MyLetPair ann var var (Expr var ann) (Expr var ann)
  | -- | a `f` b
    MyInfix ann Operator (Expr var ann) (Expr var ann)
  | -- | binder, body
    MyLambda ann var (Expr var ann)
  | -- | function, argument
    MyApp ann (Expr var ann) (Expr var ann)
  | -- | expr, thencase, elsecase
    MyIf ann (Expr var ann) (Expr var ann) (Expr var ann)
  | -- | (a,b)
    MyPair ann (Expr var ann) (Expr var ann)
  | -- | { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
    MyRecord ann (Map Name (Expr var ann))
  | -- | a.foo
    MyRecordAccess ann (Expr var ann) Name
  | -- | infix, name, expr
    MyDefineInfix ann InfixOp var (Expr var ann)
  | -- | tyName, tyArgs, Map constructor args, body
    MyData ann DataType (Expr var ann)
  | -- | use a constructor by name
    MyConstructor ann TyCon
  | -- | constructor, value
    MyConsApp ann (Expr var ann) (Expr var ann)
  | -- | expr, matches, catchAll
    MyCaseMatch
      ann
      (Expr var ann)
      (NonEmpty (TyCon, Expr var ann))
      (Maybe (Expr var ann))
  | -- | name
    MyTypedHole ann Name
  deriving (Eq, Ord, Show, Functor, Generic, JSON.FromJSON, JSON.ToJSON)

deriving instance (ToSchema var, ToSchema ann) => ToSchema (Expr var ann)

-- | Removes any annotations in the expression, useful when serialising
-- expressions
toEmptyAnnotation ::
  (Monoid b) =>
  Expr var a ->
  Expr var b
toEmptyAnnotation = fmap (const mempty)

-- | Retrieve the annotation for any given Expression
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
getAnnotation (MyTypedHole ann _) = ann
getAnnotation (MyDefineInfix ann _ _ _) = ann

-- | Map a function `f` over the expression. This function takes care of
-- recursing through the Expression
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
mapExpr _ (MyTypedHole ann a) = MyTypedHole ann a
mapExpr f (MyDefineInfix ann op bindName inExpr) =
  MyDefineInfix ann op bindName (f inExpr)

-- | Bind a function `f` over the expression. This function takes care of
-- recursing through the expression.
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
bindExpr _ (MyTypedHole ann a) = pure (MyTypedHole ann a)
bindExpr f (MyDefineInfix ann op bindName expr) =
  MyDefineInfix ann op bindName <$> f expr

-- | Given a function `f` that turns any piece of the expression in a Monoid
-- `m`, flatten the entire expression into `m`
withMonoid ::
  (Monoid m) =>
  (Expr var ann -> m) ->
  Expr var ann ->
  m
withMonoid f whole@(MyLiteral _ _) = f whole
withMonoid f whole@(MyVar _ _) = f whole
withMonoid f whole@(MyLet _ _ bindExpr' inExpr) =
  f whole
    <> withMonoid f bindExpr'
    <> withMonoid f inExpr
withMonoid f whole@(MyLetPair _ _binderA _binderB bindExpr' inExpr) =
  f whole
    <> withMonoid f bindExpr'
    <> withMonoid f inExpr
withMonoid f whole@(MyInfix _ _ a b) =
  f whole
    <> withMonoid f a
    <> withMonoid f b
withMonoid f whole@(MyLambda _ _binder expr) =
  f whole
    <> withMonoid f expr
withMonoid f whole@(MyApp _ func arg) =
  f whole
    <> withMonoid f func
    <> withMonoid f arg
withMonoid f whole@(MyIf _ matchExpr thenExpr elseExpr) =
  f whole
    <> withMonoid f matchExpr
    <> withMonoid f thenExpr
    <> withMonoid f elseExpr
withMonoid f whole@(MyPair _ a b) =
  f whole
    <> withMonoid f a
    <> withMonoid f b
withMonoid f whole@(MyRecord _ items) =
  f whole
    <> mconcat
      ( snd <$> M.toList (withMonoid f <$> items)
      )
withMonoid f whole@(MyRecordAccess _ expr _name) =
  f whole <> withMonoid f expr
withMonoid f whole@(MyData _ _ expr) =
  f whole <> withMonoid f expr
withMonoid f whole@(MyConstructor _ _) = f whole
withMonoid f whole@(MyConsApp _ func arg) =
  f whole
    <> withMonoid f func
    <> withMonoid f arg
withMonoid f whole@(MyCaseMatch _ matchExpr caseExprs catchExpr) =
  f whole
    <> withMonoid f matchExpr
    <> mconcat
      ( NE.toList
          (withMonoid f <$> (snd <$> caseExprs))
      )
    <> maybe mempty (withMonoid f) catchExpr
withMonoid f whole@MyTypedHole {} = f whole
withMonoid f whole@(MyDefineInfix _ _ _ inExpr) =
  f whole
    <> withMonoid f inExpr

data InfixBit var ann
  = IfStart (Expr var ann)
  | IfMore Operator (Expr var ann)
  deriving (Show)

getInfixList :: Expr var ann -> NE.NonEmpty (InfixBit var ann)
getInfixList expr = case expr of
  (MyInfix _ op a b) ->
    let start = getInfixList a
     in start <> NE.fromList [IfMore op b]
  other -> NE.fromList [IfStart other]

prettyInfixList :: (Show var, Printer var) => NE.NonEmpty (InfixBit var ann) -> Doc style
prettyInfixList (ifHead :| ifRest) =
  let printInfixBit (IfMore op expr') = prettyDoc op <+> printSubExpr expr'
      printInfixBit (IfStart expr') = printSubExpr expr'
   in printInfixBit ifHead <+> align (vsep (printInfixBit <$> ifRest))

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Int -> Doc style -> Doc style
indentMulti i doc = flatAlt (indent i doc) doc

prettyLet ::
  (Show var, Printer var) =>
  var ->
  Expr var ann ->
  Expr var ann ->
  Doc style
prettyLet var expr1 expr2 =
  group
    ( "let" <+> prettyDoc var
        <+> "="
        <> line
        <> indentMulti 2 (prettyDoc expr1)
        <> newlineOrIn
        <> prettyDoc expr2
    )

prettyLetPair ::
  (Show var, Printer var) =>
  var ->
  var ->
  Expr var ann ->
  Expr var ann ->
  Doc style
prettyLetPair var1 var2 expr1 body =
  group
    ( "let" <+> "(" <> prettyDoc var1 <> "," <+> prettyDoc var2
        <> ")"
        <+> "="
        <> line
        <> indentMulti 2 (printSubExpr expr1)
        <> newlineOrIn
        <> printSubExpr body
    )

newlineOrIn :: Doc style
newlineOrIn = flatAlt (";" <> line <> line) " in "

prettyDefineInfix ::
  (Printer var, Show var) =>
  InfixOp ->
  var ->
  Expr var ann ->
  Doc style
prettyDefineInfix infixOp bindName expr =
  group
    ( "infix"
        <+> prettyDoc infixOp
        <+> "="
        <+> prettyDoc bindName
        <> newlineOrIn
        <> prettyDoc expr
    )

prettyPair :: (Printer var, Show var) => Expr var ann -> Expr var ann -> Doc style
prettyPair a b =
  group
    ( "("
        <> align
          ( vsep
              [ printSubExpr a <> ",",
                printSubExpr b <> ")"
              ]
          )
    )

prettyLambda ::
  (Printer var, Show var) =>
  var ->
  Expr var ann ->
  Doc style
prettyLambda binder expr =
  group
    ( vsep
        [ "\\"
            <> prettyDoc binder
            <+> "->",
          indentMulti 2 $
            prettyDoc expr
        ]
    )

prettyRecord ::
  (Printer var, Show var) =>
  Map Name (Expr var ann) ->
  Doc style
prettyRecord map' =
  let items = M.toList map'
      printRow = \i (name, val) ->
        prettyDoc name
          <> ":"
          <+> printSubExpr val
          <> if i < length items then "," else ""
   in case items of
        [] -> "{}"
        rows ->
          let prettyRows = mapWithIndex printRow rows
           in group
                ( "{"
                    <+> align
                      ( vsep
                          prettyRows
                      )
                    <+> "}"
                )

prettyIf ::
  (Show var, Printer var) =>
  Expr var ann ->
  Expr var ann ->
  Expr var ann ->
  Doc style
prettyIf if' then' else' =
  group
    ( vsep
        [ "if"
            <+> wrapInfix if',
          "then",
          indentMulti 2 (printSubExpr then'),
          "else",
          indentMulti 2 (printSubExpr else')
        ]
    )

prettyCaseMatch ::
  (Printer var, Show var) =>
  Expr var ann ->
  NE.NonEmpty (TyCon, Expr var ann) ->
  Maybe (Expr var ann) ->
  Doc style
prettyCaseMatch sumExpr matches catchAll =
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

prettyDataType ::
  (Printer var, Show var) =>
  DataType ->
  Expr var ann ->
  Doc style
prettyDataType dt expr =
  group
    ( prettyDoc dt
        <> newlineOrIn
        <> prettyDoc expr
    )

instance (Show var, Printer var) => Printer (Expr var ann) where
  prettyDoc (MyLiteral _ l) =
    prettyDoc l
  prettyDoc (MyVar _ var) =
    prettyDoc var
  prettyDoc (MyLet _ var expr1 expr2) =
    prettyLet var expr1 expr2
  prettyDoc (MyLetPair _ var1 var2 expr1 body) =
    prettyLetPair var1 var2 expr1 body
  prettyDoc wholeExpr@MyInfix {} =
    group (prettyInfixList (getInfixList wholeExpr))
  prettyDoc (MyLambda _ binder expr) =
    prettyLambda binder expr
  prettyDoc (MyApp _ func arg) =
    printSubExpr func <> parens (printSubExpr arg)
  prettyDoc (MyRecordAccess _ expr name) =
    printSubExpr expr <> "." <> prettyDoc name
  prettyDoc (MyIf _ if' then' else') =
    prettyIf if' then' else'
  prettyDoc (MyPair _ a b) =
    prettyPair a b
  prettyDoc (MyRecord _ map') =
    prettyRecord map'
  prettyDoc (MyDefineInfix _ infixOp bindName expr) =
    prettyDefineInfix infixOp bindName expr
  prettyDoc (MyData _ dataType expr) =
    prettyDataType dataType expr
  prettyDoc (MyConstructor _ name) = prettyDoc name
  prettyDoc (MyConsApp _ fn val) = prettyDoc fn <+> wrapInfix val
  prettyDoc (MyCaseMatch _ sumExpr matches catchAll) =
    prettyCaseMatch sumExpr matches catchAll
  prettyDoc (MyTypedHole _ name) = "?" <> prettyDoc name

wrapInfix :: (Show var, Printer var) => Expr var ann -> Doc style
wrapInfix val = case val of
  val'@MyInfix {} -> inParens val'
  other -> printSubExpr other

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
