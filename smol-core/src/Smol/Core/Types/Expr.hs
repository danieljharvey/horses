{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Expr
  ( Expr (..),
    Op (..),
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Map.Strict
import qualified Data.Map.Strict as M
import Smol.Core.Helpers
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.Pattern
import Smol.Core.Types.Prim
import Smol.Core.Types.Type

data Op = OpAdd | OpEquals
  deriving stock (Eq, Ord, Show)

instance Printer Op where
  prettyDoc OpAdd = "+"
  prettyDoc OpEquals = "=="

data Expr ann
  = ELambda ann Identifier (Expr ann)
  | ELet ann Identifier (Expr ann) (Expr ann)
  | EInfix ann Op (Expr ann) (Expr ann)
  | EPrim ann Prim
  | EApp ann (Expr ann) (Expr ann)
  | EIf ann (Expr ann) (Expr ann) (Expr ann)
  | EAnn ann (Type ann) (Expr ann)
  | EVar ann Identifier
  | EConstructor ann Constructor
  | ETuple ann (Expr ann) (NE.NonEmpty (Expr ann))
  | EGlobal ann Identifier
  | EGlobalLet ann Identifier (Expr ann) (Expr ann)
  | ERecord ann (Map Identifier (Expr ann))
  | ERecordAccess ann (Expr ann) Identifier
  | EPatternMatch ann (Expr ann) (NE.NonEmpty (Pattern ann, Expr ann))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Printer (Expr ann) where
  prettyDoc = prettyExpr

------ printing shit

data InfixBit ann
  = IfStart (Expr ann)
  | IfMore Op (Expr ann)
  deriving stock (Show)

getInfixList :: Expr ann -> NE.NonEmpty (InfixBit ann)
getInfixList expr = case expr of
  (EInfix _ op a b) ->
    let start = getInfixList a
     in start <> NE.fromList [IfMore op b]
  other -> NE.fromList [IfStart other]

prettyInfixList :: NE.NonEmpty (InfixBit ann) -> PP.Doc style
prettyInfixList (ifHead NE.:| ifRest) =
  let printInfixBit (IfMore op expr') = prettyDoc op <+> printSubExpr expr'
      printInfixBit (IfStart expr') = printSubExpr expr'
   in printInfixBit ifHead <+> PP.align (PP.vsep (printInfixBit <$> ifRest))

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc = PP.flatAlt (PP.indent (fromIntegral i) doc) doc

prettyLet ::
  Identifier ->
  Expr ann ->
  Expr ann ->
  PP.Doc style
prettyLet var expr1 expr2 =
  let (args, letExpr, maybeMt) = splitExpr expr1
      prettyVar = case maybeMt of
        Just mt ->
          "(" <> prettyDoc var <> ":" <+> prettyDoc mt <> ")"
        Nothing ->
          prettyDoc var
   in PP.group
        ( "let"
            <+> prettyVar <> prettyArgs args
            <+> "="
              <> PP.line
              <> indentMulti 2 (prettyDoc letExpr)
              <> newlineOrIn
              <> prettyDoc expr2
        )
  where
    prettyArgs [] = ""
    prettyArgs as = PP.space <> PP.hsep (prettyDoc <$> as)

    splitExpr expr =
      case expr of
        (ELambda _ a rest) ->
          let (as, expr', mt) = splitExpr rest
           in ([a] <> as, expr', mt)
        (EAnn _ mt annExpr) ->
          let (as, expr', _) = splitExpr annExpr
           in (as, expr', Just mt)
        other -> ([], other, Nothing)

newlineOrIn :: PP.Doc style
newlineOrIn = PP.flatAlt (";" <> PP.line <> PP.line) " in "

prettyTuple :: Expr ann -> NE.NonEmpty (Expr ann) -> PP.Doc style
prettyTuple a as =
  PP.group
    ( "("
        <> PP.align
          ( PP.vsep
              ( PP.punctuate
                  ","
                  (printSubExpr <$> ([a] <> NE.toList as))
              )
          )
        <> ")"
    )

prettyLambda ::
  Identifier ->
  Expr ann ->
  PP.Doc style
prettyLambda binder expr =
  PP.group
    ( PP.vsep
        [ "\\"
            <> prettyDoc binder
            <+> "->",
          indentMulti 2 $
            prettyDoc expr
        ]
    )

prettyRecord ::
  Map Identifier (Expr ann) ->
  PP.Doc style
prettyRecord map' =
  let items = M.toList map'
      printRow (name, val) i =
        let item = case val of
              (EVar _ vName)
                | vName == name ->
                    prettyDoc name
              _ ->
                prettyDoc name
                  <> ":"
                  <+> printSubExpr val
         in item <> if fromIntegral i < length items then "," else ""
   in case items of
        [] -> "{}"
        rows ->
          let prettyRows = mapInd printRow rows
           in PP.group
                ( "{"
                    <+> PP.align
                      ( PP.vsep
                          prettyRows
                      )
                    <+> "}"
                )

{-
prettyArray :: [Expr ann] -> PP.Doc style
prettyArray items =
  let printRow i val =
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
-}

prettyIf ::
  Expr ann ->
  Expr ann ->
  Expr ann ->
  PP.Doc style
prettyIf if' then' else' =
  PP.group
    ( PP.vsep
        [ "if"
            <+> wrapInfix if',
          "then",
          indentMulti 2 (printSubExpr then'),
          "else",
          indentMulti 2 (printSubExpr else')
        ]
    )

prettyPatternMatch ::
  Expr ann ->
  NE.NonEmpty (Pattern ann, Expr ann) ->
  PP.Doc style
prettyPatternMatch sumExpr matches =
  "match"
    <+> printSubExpr sumExpr
    <+> "with"
    <+> PP.line
      <> PP.indent
        2
        ( PP.align $
            PP.vsep
              ( zipWith
                  (<+>)
                  (" " : repeat "|")
                  (printMatch <$> NE.toList matches)
              )
        )
  where
    printMatch (construct, expr') =
      printSubPattern construct
        <+> "->"
        <+> PP.line
          <> indentMulti 4 (printSubExpr expr')

prettyExpr :: Expr ann -> PP.Doc doc
prettyExpr (EPrim _ l) =
  prettyDoc l
prettyExpr (EAnn _ mt expr) =
  "(" <> prettyExpr expr <+> ":" <+> renderType mt <> ")"
prettyExpr (EVar _ var) =
  prettyDoc var
prettyExpr (ELet _ var expr1 expr2) =
  prettyLet var expr1 expr2
prettyExpr wholeExpr@EInfix {} =
  PP.group (prettyInfixList (getInfixList wholeExpr))
prettyExpr (ELambda _ binder expr) =
  prettyLambda binder expr
prettyExpr (EApp _ func arg) =
  prettyExpr func <+> wrapInfix arg
prettyExpr (ERecordAccess _ expr name) =
  prettyExpr expr <> "." <> prettyDoc name
prettyExpr (EIf _ if' then' else') =
  prettyIf if' then' else'
prettyExpr (ETuple _ a b) =
  prettyTuple a b
prettyExpr (ERecord _ map') =
  prettyRecord map'
prettyExpr (EConstructor _ name) =
  prettyDoc name
prettyExpr (EPatternMatch _ expr matches) =
  prettyPatternMatch expr matches
prettyExpr (EGlobal _ global) =
  prettyDoc global <> "!"
prettyExpr _ = error "missing globals pretty printer"

wrapInfix :: Expr ann -> PP.Doc style
wrapInfix val = case val of
  val'@EInfix {} -> inParens val'
  other -> printSubExpr other

inParens :: Expr ann -> PP.Doc style
inParens = PP.parens . prettyExpr

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: Expr ann -> PP.Doc style
printSubExpr expr = case expr of
  all'@ELet {} -> inParens all'
  all'@ELambda {} -> inParens all'
  all'@EIf {} -> inParens all'
  all'@EApp {} -> inParens all'
  all'@ETuple {} -> inParens all'
  all'@EPatternMatch {} -> inParens all'
  a -> prettyDoc a
