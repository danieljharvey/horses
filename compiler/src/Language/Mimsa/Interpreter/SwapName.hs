module Language.Mimsa.Interpreter.SwapName
  ( swapName,
  )
where

import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- step through Expr, replacing vars with numbered variables
swapName ::
  Variable ->
  Variable ->
  Expr Variable ann ->
  Expr Variable ann
swapName from to (MyVar ann from') =
  if from == from'
    then MyVar ann to
    else MyVar ann from'
swapName from to (MyLet ann ident expr body) =
  MyLet ann (swapInIdent from to ident) (swapName from to expr) (swapName from to body)
swapName from to (MyLetPattern ann pat expr body) =
  MyLetPattern ann (swapPatternName from to pat) (swapName from to expr) (swapName from to body)
swapName from to expr =
  mapExpr (swapName from to) expr

swapPatternName ::
  Variable ->
  Variable ->
  Pattern Variable ann ->
  Pattern Variable ann
swapPatternName from to (PVar ann from') =
  if from == from'
    then PVar ann to
    else PVar ann from'
swapPatternName _ _ other = other

swapInIdent :: Variable -> Variable -> Identifier Variable ann -> Identifier Variable ann
swapInIdent from to (Identifier ann from') =
  if from == from'
    then Identifier ann to
    else Identifier ann from'
swapInIdent from to (AnnotatedIdentifier ann from') =
  if from == from'
    then AnnotatedIdentifier ann to
    else AnnotatedIdentifier ann from'
