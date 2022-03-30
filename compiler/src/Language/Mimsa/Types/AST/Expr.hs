{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Types.AST.Expr
  ( Expr (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.Bifunctor.TH
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType (DataType)
import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.AST.Literal (Literal)
import Language.Mimsa.Types.AST.Operator
import Language.Mimsa.Types.AST.Pattern
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Utils
import Prettyprinter

-------

-- |
-- The main expression type that we parse from syntax
-- `var` is the type of variables. When we parse them they are
-- string-based `Name`, but after substitution they become a `Variable`
-- which is either a string or a numbered variable
data Expr var ann
  = -- | a literal, such as String, Int, Boolean
    MyLiteral
      { expAnn :: ann,
        expLit :: Literal
      }
  | -- | a named variable
    MyVar
      { expAnn :: ann,
        expVar :: var
      }
  | -- | binder, expr, body
    MyLet
      { expAnn :: ann,
        expBinder :: Identifier var ann,
        expExpr :: Expr var ann,
        expBody :: Expr var ann
      }
  | -- | pat, expr, body
    MyLetPattern
      { expAnn :: ann,
        expPattern :: Pattern var ann,
        expExpr :: Expr var ann,
        expBody :: Expr var ann
      }
  | -- | a `f` b
    MyInfix
      { expAnn :: ann,
        expOperator :: Operator,
        expExpr :: Expr var ann,
        expBody :: Expr var ann
      }
  | -- | binder, body
    MyLambda
      { expAnn :: ann,
        expBinder :: Identifier var ann,
        expBody :: Expr var ann
      }
  | -- | function, argument
    MyApp
      { expAnn :: ann,
        expFunc :: Expr var ann,
        expArg :: Expr var ann
      }
  | -- | expr, thencase, elsecase
    MyIf
      { expAnn :: ann,
        expPred :: Expr var ann,
        expThen :: Expr var ann,
        expElse :: Expr var ann
      }
  | -- | (a,b)
    MyPair
      { expAnn :: ann,
        expA :: Expr var ann,
        expB :: Expr var ann
      }
  | -- | { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
    MyRecord
      { expAnn :: ann,
        expRecordItems :: Map Name (Expr var ann)
      }
  | -- | a.foo
    MyRecordAccess
      { expAnn :: ann,
        expRecord :: Expr var ann,
        expKey :: Name
      }
  | MyArray
      { expAnn :: ann,
        expArrayItems :: [Expr var ann]
      }
  | -- | infix, func expr, expr
    MyDefineInfix
      { expAnn :: ann,
        expInfixOp :: InfixOp,
        expInfixFunc :: Expr var ann,
        expBody :: Expr var ann
      }
  | -- | tyName, tyArgs, Map constructor args, body
    MyData
      { expAnn :: ann,
        expDataType :: DataType,
        expBody :: Expr var ann
      }
  | -- | use a constructor by name
    MyConstructor
      { expAnn :: ann,
        expTyCon :: TyCon
      }
  | -- | expr, [(pattern, expr)]
    MyPatternMatch
      { expAnn :: ann,
        expExpr :: Expr var ann,
        expPatterns :: [(Pattern var ann, Expr var ann)]
      }
  | -- | name
    MyTypedHole {expAnn :: ann, expTypedHoleName :: Name}
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

$(deriveBifunctor ''Expr)

data InfixBit var ann
  = IfStart (Expr var ann)
  | IfMore Operator (Expr var ann)
  deriving stock (Show)

getInfixList :: Expr Name ann -> NE.NonEmpty (InfixBit Name ann)
getInfixList expr = case expr of
  (MyInfix _ op a b) ->
    let start = getInfixList a
     in start <> NE.fromList [IfMore op b]
  other -> NE.fromList [IfStart other]

prettyInfixList :: NE.NonEmpty (InfixBit Name ann) -> Doc style
prettyInfixList (ifHead :| ifRest) =
  let printInfixBit (IfMore op expr') = prettyDoc op <+> printSubExpr expr'
      printInfixBit (IfStart expr') = printSubExpr expr'
   in printInfixBit ifHead <+> align (vsep (printInfixBit <$> ifRest))

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Int -> Doc style -> Doc style
indentMulti i doc = flatAlt (indent i doc) doc

prettyLet ::
  Identifier Name ann ->
  Expr Name ann ->
  Expr Name ann ->
  Doc style
prettyLet var expr1 expr2 =
  let (args, letExpr) = splitExpr expr1
   in group
        ( "let" <+> prettyDoc var <> prettyArgs args
            <+> "="
            <> line
            <> indentMulti 2 (prettyDoc letExpr)
            <> newlineOrIn
            <> prettyDoc expr2
        )
  where
    prettyArgs [] = ""
    prettyArgs as = space <> hsep (prettyDoc <$> as)

    splitExpr expr =
      case expr of
        (MyLambda _ a rest) ->
          let (as, expr') = splitExpr rest
           in ([a] <> as, expr')
        other -> ([], other)

prettyLetPattern ::
  Pattern Name ann ->
  Expr Name ann ->
  Expr Name ann ->
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
  InfixOp ->
  Expr Name ann ->
  Expr Name ann ->
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

prettyPair :: Expr Name ann -> Expr Name ann -> Doc style
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
  Identifier Name ann ->
  Expr Name ann ->
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
  Map Name (Expr Name ann) ->
  Doc style
prettyRecord map' =
  let items = M.toList map'
      printRow i (name, val) =
        let item = case val of
              (MyVar _ vName)
                | vName == name ->
                  prettyDoc name
              _ ->
                prettyDoc name
                  <> ":"
                  <+> printSubExpr val
         in item <> if i < length items then "," else ""
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

prettyArray :: [Expr Name ann] -> Doc style
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

prettyIf ::
  Expr Name ann ->
  Expr Name ann ->
  Expr Name ann ->
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
  Expr Name ann ->
  [(Pattern Name ann, Expr Name ann)] ->
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
      printSubPattern construct <+> "->" <+> line
        <> indentMulti 4 (printSubExpr expr')

prettyDataType ::
  DataType ->
  Expr Name ann ->
  Doc style
prettyDataType dt expr =
  group
    ( prettyDoc dt
        <> newlineOrIn
        <> prettyDoc expr
    )

-- just for debugging
instance (Printer var) => Printer (Expr (var, a) ann) where
  prettyDoc = prettyDoc . first (mkName . prettyPrint . fst)

instance Printer (Expr Name ann) where
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
    prettyDoc expr <> "." <> prettyDoc name
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

wrapInfix :: Expr Name ann -> Doc style
wrapInfix val = case val of
  val'@MyInfix {} -> inParens val'
  other -> printSubExpr other

inParens :: Expr Name ann -> Doc style
inParens = parens . prettyDoc

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: Expr Name ann -> Doc style
printSubExpr expr = case expr of
  all'@MyLet {} -> inParens all'
  all'@MyLambda {} -> inParens all'
  all'@MyIf {} -> inParens all'
  all'@MyApp {} -> inParens all'
  all'@MyPair {} -> inParens all'
  a -> prettyDoc a
