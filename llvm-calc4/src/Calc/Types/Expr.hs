{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Expr (Expr (..), Op (..)) where

import Calc.Types.FunctionName
import Calc.Types.Identifier
import Calc.Types.Pattern
import Calc.Types.Prim
import qualified Data.List.NonEmpty as NE
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

data Expr ann
  = EPrim ann Prim
  | EInfix ann Op (Expr ann) (Expr ann)
  | EIf ann (Expr ann) (Expr ann) (Expr ann)
  | EVar ann Identifier
  | EApply ann FunctionName [Expr ann]
  | ETuple ann (Expr ann) (NE.NonEmpty (Expr ann))
  | EPatternMatch ann (Expr ann) (NE.NonEmpty (Pattern ann, Expr ann))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc =
  PP.flatAlt (PP.indent (fromIntegral i) doc) doc

-- | this instance defines how to nicely print `Expr`
instance PP.Pretty (Expr ann) where
  pretty (EPrim _ prim) =
    PP.pretty prim
  pretty (EInfix _ op a b) =
    PP.pretty a <+> PP.pretty op <+> PP.pretty b
  pretty (EIf _ predExpr thenExpr elseExpr) =
    "if" <+> PP.pretty predExpr <+> "then" <+> indentMulti 2 (PP.pretty thenExpr) <+> "else" <+> indentMulti 2 (PP.pretty elseExpr)
  pretty (EVar _ ident) = PP.pretty ident
  pretty (EApply _ fn args) = PP.pretty fn <> "(" <> PP.cat pArgs <> ")"
    where
      pArgs = PP.punctuate "," (PP.pretty <$> args)
  pretty (ETuple _ a as) =
    "(" <> PP.cat (PP.punctuate "," (PP.pretty <$> tupleItems a as)) <> ")"
    where
      tupleItems :: a -> NE.NonEmpty a -> [a]
      tupleItems b bs = b : NE.toList bs
  pretty (EPatternMatch _ matchExp patterns) =
    prettyPatternMatch matchExp patterns

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
      PP.pretty construct
        <+> "->"
        <+> PP.line
          <> indentMulti 4 (printSubExpr expr')

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: Expr ann -> PP.Doc style
printSubExpr expr = case expr of
  all'@EIf {} -> inParens all'
  all'@EApply {} -> inParens all'
  all'@ETuple {} -> inParens all'
  all'@EPatternMatch {} -> inParens all'
  a -> PP.pretty a

inParens :: Expr ann -> PP.Doc style
inParens = PP.parens . PP.pretty

data Op
  = OpAdd
  | OpMultiply
  | OpSubtract
  | OpEquals
  deriving stock (Eq, Ord, Show)

-- how to print `Op` values
instance PP.Pretty Op where
  pretty OpAdd = "+"
  pretty OpMultiply = "*"
  pretty OpSubtract = "-"
  pretty OpEquals = "=="
