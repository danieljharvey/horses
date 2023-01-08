module ExprUtils
  ( typeIsStruct,
    mapOuterExprAnnotation,
    mapExpr,
    mapPattern,
    patternMonoid,
  )
where

import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import Types

-- helper functions for manipulating Expr types
--
typeIsStruct :: Type ann -> Bool
typeIsStruct TPrim {} = False
typeIsStruct TLiteral {} = False
typeIsStruct _ = True

-- | modify the outer annotation of an expression
-- useful for adding line numbers during parsing
mapOuterExprAnnotation :: (ann -> ann) -> Expr ann -> Expr ann
mapOuterExprAnnotation f expr' =
  case expr' of
    EInfix ann a b c -> EInfix (f ann) a b c
    EAnn ann expr mt -> EAnn (f ann) expr mt
    EPrim ann a -> EPrim (f ann) a
    EVar ann a -> EVar (f ann) a
    EConstructor ann a -> EConstructor (f ann) a
    ELet ann a b c -> ELet (f ann) a b c
    ELambda ann a b -> ELambda (f ann) a b
    EApp ann a b -> EApp (f ann) a b
    EIf ann a b c -> EIf (f ann) a b c
    ETuple ann a b -> ETuple (f ann) a b
    EGlobal ann a -> EGlobal (f ann) a
    EGlobalLet ann a b c -> EGlobalLet (f ann) a b c
    ERecord ann a -> ERecord (f ann) a
    ERecordAccess ann b c -> ERecordAccess (f ann) b c
    EPatternMatch ann a b -> EPatternMatch (f ann) a b

mapExpr :: (Expr ann -> Expr ann) -> Expr ann -> Expr ann
mapExpr f (EInfix ann op a b) = EInfix ann op (f a) (f b)
mapExpr f (EAnn ann mt expr) = EAnn ann mt (f expr)
mapExpr _ (EPrim ann a) = EPrim ann a
mapExpr _ (EVar ann a) = EVar ann a
mapExpr _ (EConstructor ann a) = EConstructor ann a
mapExpr f (ELet ann ident expr rest) = ELet ann ident (f expr) (f rest)
mapExpr f (ELambda ann ident body) = ELambda ann ident (f body)
mapExpr f (EApp ann fn arg) = EApp ann (f fn) (f arg)
mapExpr f (EIf ann predExp thenExp elseExp) =
  EIf ann (f predExp) (f thenExp) (f elseExp)
mapExpr f (ETuple ann a as) = ETuple ann (f a) (f <$> as)
mapExpr _ (EGlobal ann ident) = EGlobal ann ident
mapExpr f (EGlobalLet ann ident expr rest) =
  EGlobalLet ann ident (f expr) (f rest)
mapExpr f (ERecord ann as) = ERecord ann (f <$> as)
mapExpr f (ERecordAccess ann expr ident) =
  ERecordAccess ann (f expr) ident
mapExpr f (EPatternMatch ann patExpr pats) =
  EPatternMatch ann (f patExpr) (second f <$> pats)

mapPattern :: (Pattern ann -> Pattern ann) -> Pattern ann -> Pattern ann
mapPattern _ (PLiteral ann l) = PLiteral ann l
mapPattern _ (PWildcard a) = PWildcard a
mapPattern _ (PVar ann a) = PVar ann a
mapPattern f (PTuple ann a as) = PTuple ann (f a) (f <$> as)
mapPattern f (PConstructor ann constructor as) =
  PConstructor ann constructor (f <$> as)

patternMonoid :: (Monoid a) => (Pattern ann -> a) -> Pattern ann -> a
patternMonoid _ PLiteral {} = mempty
patternMonoid _ PWildcard {} = mempty
patternMonoid _ PVar {} = mempty
patternMonoid f (PTuple _ a as) =
  f a <> foldMap f (NE.toList as)
patternMonoid f (PConstructor _ _ as) =
  foldMap f as
