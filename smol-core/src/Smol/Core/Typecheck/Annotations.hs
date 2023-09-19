{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Annotations
  ( getExprAnnotation,
    getPatternAnnotation,
    getSpreadAnnotation,
    getTypeAnnotation,
  )
where

import Smol.Core.Types

getExprAnnotation :: Expr dep ann -> ann
getExprAnnotation (EInfix ann _ _ _) = ann
getExprAnnotation (EConstructor ann _) = ann
getExprAnnotation (ELet ann _ _ _) = ann
getExprAnnotation (ELambda ann _ _) = ann
getExprAnnotation (EPrim ann _) = ann
getExprAnnotation (EApp ann _ _) = ann
getExprAnnotation (EIf ann _ _ _) = ann
getExprAnnotation (EAnn ann _ _) = ann
getExprAnnotation (EVar ann _) = ann
getExprAnnotation (ETuple ann _ _) = ann
getExprAnnotation (EArray ann _) = ann
getExprAnnotation (ERecord ann _) = ann
getExprAnnotation (ERecordAccess ann _ _) = ann
getExprAnnotation (EPatternMatch ann _ _) = ann

getPatternAnnotation :: Pattern dep ann -> ann
getPatternAnnotation (PVar ann _) = ann
getPatternAnnotation (PWildcard ann) = ann
getPatternAnnotation (PTuple ann _ _) = ann
getPatternAnnotation (PLiteral ann _) = ann
getPatternAnnotation (PConstructor ann _ _) = ann
getPatternAnnotation (PArray ann _ _) = ann

getSpreadAnnotation :: Spread dep ann -> Maybe ann
getSpreadAnnotation NoSpread = Nothing
getSpreadAnnotation (SpreadValue ann _) = Just ann
getSpreadAnnotation (SpreadWildcard ann) = Just ann

getTypeAnnotation :: Type dep ann -> ann
getTypeAnnotation (TPrim ann _) = ann
getTypeAnnotation (TInfix ann _ _ _) = ann
getTypeAnnotation (TUnknown ann _) = ann
getTypeAnnotation (TConstructor ann _) = ann
getTypeAnnotation (TApp ann _ _) = ann
getTypeAnnotation (TFunc ann _ _ _) = ann
getTypeAnnotation (TTuple ann _ _) = ann
getTypeAnnotation (TArray ann _ _) = ann
getTypeAnnotation (TVar ann _) = ann
getTypeAnnotation (TLiteral ann _) = ann
getTypeAnnotation (TRecord ann _) = ann

