{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Mimsa.Types.AST.Expr
  ( Expr (..),
  )
where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi (ToSchema)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType (DataType)
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.AST.Literal (Literal)
import Language.Mimsa.Types.AST.Operator
import Language.Mimsa.Types.AST.Pattern
import Language.Mimsa.Types.Identifiers (Name, TyCon)
import Language.Mimsa.Utils

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
  | -- | pat, expr, body
    MyLetPattern ann (Pattern var ann) (Expr var ann) (Expr var ann)
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
  | MyArray ann [Expr var ann]
  | -- | infix, func expr, expr
    MyDefineInfix ann InfixOp (Expr var ann) (Expr var ann)
  | -- | tyName, tyArgs, Map constructor args, body
    MyData ann DataType (Expr var ann)
  | -- | use a constructor by name
    MyConstructor ann TyCon
  | -- | expr, [(pattern, expr)]
    MyPatternMatch
      ann
      (Expr var ann)
      [(Pattern var ann, Expr var ann)]
  | -- | name
    MyTypedHole ann Name
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

deriving anyclass instance
  (ToSchema var, ToSchema ann, JSON.ToJSONKey var) =>
  ToSchema (Expr var ann)

data InfixBit var ann
  = IfStart (Expr var ann)
  | IfMore Operator (Expr var ann)
  deriving stock (Show)

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

prettyLetPattern ::
  (Show var, Printer var) =>
  Pattern var ann ->
  Expr var ann ->
  Expr var ann ->
  Doc style
prettyLetPattern pat expr body =
  group
    ( "let" <+> printSubPattern pat
        <+> "="
        <> line
        <> indentMulti 2 (printSubExpr expr)
        <> newlineOrIn
        <> printSubExpr body
    )

newlineOrIn :: Doc style
newlineOrIn = flatAlt (";" <> line <> line) " in "

prettyDefineInfix ::
  (Printer var, Show var) =>
  InfixOp ->
  Expr var ann ->
  Expr var ann ->
  Doc style
prettyDefineInfix infixOp bindExpr expr =
  group
    ( "infix"
        <+> prettyDoc infixOp
        <+> "="
        <+> prettyDoc bindExpr
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

prettyArray :: (Show var, Printer var) => [Expr var ann] -> Doc style
prettyArray items =
  let printRow = \i val ->
        printSubExpr val
          <> if i < length items then "," else ""
   in case items of
        [] -> "[]"
        rows ->
          let prettyRows = mapWithIndex printRow rows
           in group
                ( "["
                    <+> align
                      ( vsep
                          prettyRows
                      )
                    <+> "]"
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

prettyPatternMatch ::
  (Printer var, Show var) =>
  Expr var ann ->
  [(Pattern var ann, Expr var ann)] ->
  Doc style
prettyPatternMatch sumExpr matches =
  "match"
    <+> printSubExpr sumExpr
    <+> "with"
    <+> line
      <> indent
        2
        ( align $
            vsep
              ( zipWith
                  (<+>)
                  (" " : repeat "|")
                  (printMatch <$> matches)
              )
        )
  where
    printMatch (construct, expr') =
      printSubPattern construct <+> "->" <+> printSubExpr expr'

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
  prettyDoc (MyLetPattern _ pat expr body) =
    prettyLetPattern pat expr body
  prettyDoc wholeExpr@MyInfix {} =
    group (prettyInfixList (getInfixList wholeExpr))
  prettyDoc (MyLambda _ binder expr) =
    prettyLambda binder expr
  prettyDoc (MyApp _ func arg) =
    prettyDoc func <+> wrapInfix arg
  prettyDoc (MyRecordAccess _ expr name) =
    printSubExpr expr <> "." <> prettyDoc name
  prettyDoc (MyIf _ if' then' else') =
    prettyIf if' then' else'
  prettyDoc (MyPair _ a b) =
    prettyPair a b
  prettyDoc (MyRecord _ map') =
    prettyRecord map'
  prettyDoc (MyArray _ items) = prettyArray items
  prettyDoc (MyDefineInfix _ infixOp bindExpr expr) =
    prettyDefineInfix infixOp bindExpr expr
  prettyDoc (MyData _ dataType expr) =
    prettyDataType dataType expr
  prettyDoc (MyConstructor _ name) = prettyDoc name
  prettyDoc (MyTypedHole _ name) = "?" <> prettyDoc name
  prettyDoc (MyPatternMatch _ expr matches) =
    prettyPatternMatch expr matches

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
  all'@MyApp {} -> inParens all'
  all'@MyPair {} -> inParens all'
  a -> prettyDoc a
