{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Types.Expr
  ( Expr (..),
    ParsedExpr,
    ResolvedExpr,
    IdentityExpr,
  )
where

import Control.Monad.Identity
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Smol.Core.Helpers
import Smol.Core.Printer
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.Op
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Pattern
import Smol.Core.Types.Prim
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

---------------------------

type ParsedExpr ann = Expr ParseDep ann

---------------------------

type ResolvedExpr ann = Expr ResolvedDep ann

---------------------------

type IdentityExpr ann = Expr Identity ann

---------------------------

data Expr dep ann
  = ELambda ann (dep Identifier) (Expr dep ann)
  | ELet ann (dep Identifier) (Expr dep ann) (Expr dep ann)
  | EInfix ann Op (Expr dep ann) (Expr dep ann)
  | EPrim ann Prim
  | EApp ann (Expr dep ann) (Expr dep ann)
  | EIf ann (Expr dep ann) (Expr dep ann) (Expr dep ann)
  | EAnn ann (Type dep ann) (Expr dep ann)
  | EVar ann (dep Identifier)
  | EConstructor ann (dep Constructor)
  | ETuple ann (Expr dep ann) (NE.NonEmpty (Expr dep ann))
  | EArray ann (Seq (Expr dep ann))
  | ERecord ann (Map Identifier (Expr dep ann))
  | ERecordAccess ann (Expr dep ann) Identifier
  | EPatternMatch
      ann
      (Expr dep ann)
      ( NE.NonEmpty
          (Pattern dep ann, Expr dep ann)
      )
  deriving stock (Functor, Foldable, Generic, Traversable)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep Constructor),
    Eq (dep TypeName)
  ) =>
  Eq (Expr dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier),
    Ord (dep Constructor),
    Ord (dep TypeName)
  ) =>
  Ord (Expr dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier),
    Show (dep Constructor),
    Show (dep TypeName)
  ) =>
  Show (Expr dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (Expr dep ann)

deriving anyclass instance
  ( Ord (dep Identifier),
    FromJSONKey (dep Identifier),
    FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep Constructor),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (Expr dep ann)

instance
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Printer (Expr dep ann)
  where
  prettyDoc = prettyExpr

------ printing shit

data InfixBit dep ann
  = IfStart (Expr dep ann)
  | IfMore Op (Expr dep ann)

deriving stock instance
  ( Show ann,
    Show (dep TypeName),
    Show (dep Constructor),
    Show (dep Identifier)
  ) =>
  Show (InfixBit dep ann)

getInfixList :: Expr dep ann -> NE.NonEmpty (InfixBit dep ann)
getInfixList expr = case expr of
  (EInfix _ op a b) ->
    let start = getInfixList a
     in start <> NE.fromList [IfMore op b]
  other -> NE.fromList [IfStart other]

prettyInfixList ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  NE.NonEmpty (InfixBit dep ann) ->
  PP.Doc style
prettyInfixList (ifHead NE.:| ifRest) =
  let printInfixBit (IfMore op expr') = prettyDoc op <+> printSubExpr expr'
      printInfixBit (IfStart expr') = printSubExpr expr'
   in printInfixBit ifHead <+> PP.align (PP.vsep (printInfixBit <$> ifRest))

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc = PP.flatAlt (PP.indent (fromIntegral i) doc) doc

prettyLet ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  dep Identifier ->
  Expr dep ann ->
  Expr dep ann ->
  PP.Doc style
prettyLet var expr1 expr2 =
  let (args, letExpr) = splitExpr expr1
   in PP.group
        ( "let"
            <+> prettyDoc var
            <> prettyArgs args
            <+> "="
            <+> PP.line'
            <> exprInBlock letExpr
            <> newlineOrIn
            <> prettyDoc expr2
        )
  where
    prettyArgs [] = ""
    prettyArgs as = PP.space <> PP.hsep (prettyDoc <$> as)

    splitExpr expr =
      case expr of
        (ELambda _ a rest) ->
          let (as, expr') = splitExpr rest
           in ([a] <> as, expr')
        other -> ([], other)

-- when printing let bindings or exprs in a pattern,
-- wrap stuff with let bindings inside `{` `}`
exprInBlock ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  PP.Doc style
exprInBlock expr@(ELet {}) = "{" <> PP.line <+> indentMulti 2 (prettyDoc expr) <+> PP.line <> "}"
exprInBlock expr = indentMulti 2 (prettyDoc expr)

newlineOrIn :: PP.Doc style
newlineOrIn = PP.flatAlt (";" <> PP.line <> PP.line') " in "

prettyTuple ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  NE.NonEmpty (Expr dep ann) ->
  PP.Doc style
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

prettyPatternLambda ::
  (Eq (dep Identifier), Printer (dep Constructor), Printer (dep Identifier), Printer (dep TypeName)) =>
  Pattern dep ann ->
  Expr dep ann ->
  PP.Doc style
prettyPatternLambda pat patExpr =
  PP.group
    ( PP.vsep
        [ "\\"
            <> prettyDoc pat
            <+> "->",
          indentMulti 2 $
            prettyDoc patExpr
        ]
    )

prettyLambda ::
  (Eq (dep Identifier), Printer (dep Constructor), Printer (dep Identifier), Printer (dep TypeName)) =>
  dep Identifier ->
  Expr dep ann ->
  PP.Doc style
prettyLambda binder (EPatternMatch _ (EVar _ matchBinder) ((pat, patExpr) NE.:| []))
  | binder == matchBinder =
      prettyPatternLambda pat patExpr
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
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Map Identifier (Expr dep ann) ->
  PP.Doc style
prettyRecord map' =
  let items = M.toList map'
      printRow (name, val) i =
        let item =
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

prettyIf ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer
      ( dep Identifier
      ),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  Expr dep ann ->
  Expr dep ann ->
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

prettyLetPattern ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  Pattern dep ann ->
  Expr dep ann ->
  PP.Doc style
prettyLetPattern sumExpr pat patExpr =
  "let"
    <+> prettyDoc pat
    <+> "="
    <+> printSubExpr sumExpr
    <> newlineOrIn
    <> prettyDoc patExpr

prettyPatternMatch ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  NE.NonEmpty (Pattern dep ann, Expr dep ann) ->
  PP.Doc style
prettyPatternMatch sumExpr ((pat, patExpr) NE.:| []) =
  prettyLetPattern sumExpr pat patExpr
prettyPatternMatch sumExpr matches =
  "case"
    <+> printSubExpr sumExpr
    <+> "{"
    <> PP.line
    <> indentMulti
      2
      ( PP.align $
          PP.vsep (printMatch <$> addNums matches)
      )
    <> PP.line
    <> "}"
  where
    addNums :: NE.NonEmpty a -> [(Int, a)]
    addNums = zip [1 ..] . NE.toList

    printMatch (index, (construct, expr')) =
      printSubPattern construct
        <+> "->"
        <> PP.line'
        <+> exprInBlock expr'
        <> if index < length matches then "," else ""

prettyArray ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Seq (Expr dep ann) ->
  PP.Doc style
prettyArray items =
  let printRow val i =
        printSubExpr val
          <> if i < fromIntegral (length items) then "," else ""
   in case items of
        rows
          | not (Seq.null rows) ->
              let prettyRows = mapInd printRow (toList rows)
               in PP.group
                    ( "["
                        <+> PP.align
                          ( PP.vsep
                              prettyRows
                          )
                        <+> "]"
                    )
        _ -> "[]"

prettyExpr ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  PP.Doc doc
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
prettyExpr (EArray _ as) =
  prettyArray as

wrapInfix ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  PP.Doc style
wrapInfix val = case val of
  val'@EInfix {} -> inParens val'
  other -> printSubExpr other

inParens ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  PP.Doc style
inParens = PP.parens . prettyExpr

-- print simple things with no brackets, and complex things inside brackets
printSubExpr ::
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  PP.Doc style
printSubExpr expr = case expr of
  all'@ELet {} -> inParens all'
  all'@ELambda {} -> inParens all'
  all'@EApp {} -> inParens all'
  all'@ETuple {} -> inParens all'
  a -> prettyDoc a
