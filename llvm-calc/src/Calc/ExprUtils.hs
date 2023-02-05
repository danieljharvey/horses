{-# LANGUAGE RankNTypes #-}

module Calc.ExprUtils
  (
    mapOuterExprAnnotation,
    mapExpr





  )
where

import Calc.Types

-- | modify the outer annotation of an expression
-- useful for adding line numbers during parsing
mapOuterExprAnnotation :: (ann -> ann) -> Expr  ann -> Expr  ann
mapOuterExprAnnotation f expr' =
  case expr' of
    EInfix ann a b c -> EInfix (f ann) a b c
    EPrim ann a -> EPrim (f ann) a

mapExpr :: (Expr  ann -> Expr  ann) -> Expr  ann -> Expr  ann
mapExpr f (EInfix ann op a b) = EInfix ann op (f a) (f b)
mapExpr _ (EPrim ann a) = EPrim ann a
