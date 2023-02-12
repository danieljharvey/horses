{-# LANGUAGE RankNTypes #-}

module Calc.ExprUtils
  ( mapOuterExprAnnotation,
    mapExpr,
    getOuterAnnotation
  )
where

import Calc.Types

-- | get the annotation in the first leaf found in an `Expr`.
-- useful for getting the overall type of an expression
getOuterAnnotation :: Expr ann -> ann
getOuterAnnotation (EInfix ann _ _ _) = ann
getOuterAnnotation (EPrim ann _) = ann

-- | modify the outer annotation of an expression
-- useful for adding line numbers during parsing
mapOuterExprAnnotation :: (ann -> ann) -> Expr ann -> Expr ann
mapOuterExprAnnotation f expr' =
  case expr' of
    EInfix ann a b c -> EInfix (f ann) a b c
    EPrim ann a -> EPrim (f ann) a

-- | Given a function that changes `Expr` values, apply it throughout
-- an AST tree
mapExpr :: (Expr ann -> Expr ann) -> Expr ann -> Expr ann
mapExpr f (EInfix ann op a b) = EInfix ann op (f a) (f b)
mapExpr _ (EPrim ann a) = EPrim ann a
