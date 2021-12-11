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

-- | TODO: recurse through all cases
swapPatternName ::
  Variable ->
  Variable ->
  Pattern Variable ann ->
  Pattern Variable ann
swapPatternName from to pat =
  case pat of
    (PVar ann from') ->
      if from == from'
        then PVar ann to
        else PVar ann from'
    (PWildcard ann) ->
      PWildcard ann
    (PLit ann l) -> PLit ann l
    (PConstructor ann c d) -> PConstructor ann c d
    (PPair ann a b) ->
      PPair
        ann
        (swapPatternName from to a)
        (swapPatternName from to b)
    (PRecord ann as) ->
      PRecord ann (swapPatternName from to <$> as)
    (PArray ann as a) ->
      PArray
        ann
        (swapPatternName from to <$> as)
        (swapSpreadName from to a)
    (PString ann as a) ->
      PString
        ann
        (swapStringPartName from to as)
        (swapStringPartName from to a)

swapSpreadName ::
  Variable ->
  Variable ->
  Spread Variable ann ->
  Spread Variable ann
swapSpreadName from to (SpreadValue ann from') =
  if from == from'
    then SpreadValue ann to
    else SpreadValue ann from'
swapSpreadName _ _ a = a

swapStringPartName ::
  Variable ->
  Variable ->
  StringPart Variable ann ->
  StringPart Variable ann
swapStringPartName from to (StrValue ann from') =
  if from == from'
    then StrValue ann to
    else StrValue ann from'
swapStringPartName _ _ a = a

swapInIdent :: Variable -> Variable -> Identifier Variable ann -> Identifier Variable ann
swapInIdent from to (Identifier ann from') =
  if from == from'
    then Identifier ann to
    else Identifier ann from'
swapInIdent from to (AnnotatedIdentifier ann from') =
  if from == from'
    then AnnotatedIdentifier ann to
    else AnnotatedIdentifier ann from'
