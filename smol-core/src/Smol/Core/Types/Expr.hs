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
import Data.Aeson (FromJSON, ToJSON)
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
  | EGlobal ann Identifier
  | EGlobalLet ann Identifier (Expr dep ann) (Expr dep ann)
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
  ( ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (Expr dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep Constructor),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (Expr dep ann)

instance
  ( Printer (dep Constructor),
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
  (Printer (dep Constructor), Printer (dep Identifier), Printer (dep TypeName)) =>
  NE.NonEmpty (InfixBit dep ann) ->
  PP.Doc style
prettyInfixList (ifHead NE.:| ifRest) =
  let printInfixBit (IfMore op expr') = prettyDoc op <+> printSubExpr expr'
      printInfixBit (IfStart expr') = printSubExpr expr'
   in printInfixBit ifHead <+> PP.align (PP.vsep (printInfixBit <$> ifRest))

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc = PP.flatAlt (PP.indent (fromIntegral i) doc) doc

prettyGlobalLet ::
  ( Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Identifier ->
  Expr dep ann ->
  Expr dep ann ->
  PP.Doc style
prettyGlobalLet var expr1 expr2 =
  let (args, letExpr, maybeMt) = splitExpr expr1
      prettyVar = case maybeMt of
        Just mt ->
          "(" <> prettyDoc var <> ":" <+> prettyDoc mt <> ")"
        Nothing ->
          prettyDoc var
   in PP.group
        ( "global"
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

prettyLet ::
  ( Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  dep Identifier ->
  Expr dep ann ->
  Expr dep ann ->
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

prettyTuple ::
  ( Printer (dep Constructor),
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

prettyLambda ::
  (Printer (dep Constructor), Printer (dep Identifier), Printer (dep TypeName)) =>
  dep Identifier ->
  Expr dep ann ->
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
  (Printer (dep Constructor), Printer (dep Identifier), Printer (dep TypeName)) =>
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
  ( Printer (dep Constructor),
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

prettyPatternMatch ::
  (Printer (dep Constructor), Printer (dep Identifier), Printer (dep TypeName)) =>
  Expr dep ann ->
  NE.NonEmpty (Pattern dep ann, Expr dep ann) ->
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

prettyArray ::
  ( Printer (dep Constructor),
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
  ( Printer (dep Constructor),
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
prettyExpr (EGlobal _ global) =
  prettyDoc global <> "!"
prettyExpr (EGlobalLet _ var expr1 expr2) =
  prettyGlobalLet var expr1 expr2
prettyExpr (EArray _ as) =
  prettyArray as

wrapInfix ::
  ( Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  PP.Doc style
wrapInfix val = case val of
  val'@EInfix {} -> inParens val'
  other -> printSubExpr other

inParens ::
  ( Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  PP.Doc style
inParens = PP.parens . prettyExpr

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: (Printer (dep Constructor), Printer (dep Identifier), Printer (dep TypeName)) => Expr dep ann -> PP.Doc style
printSubExpr expr = case expr of
  all'@ELet {} -> inParens all'
  all'@ELambda {} -> inParens all'
  all'@EIf {} -> inParens all'
  all'@EApp {} -> inParens all'
  all'@ETuple {} -> inParens all'
  all'@EPatternMatch {} -> inParens all'
  a -> prettyDoc a
