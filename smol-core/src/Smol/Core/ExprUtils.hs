{-# LANGUAGE RankNTypes #-}

module Smol.Core.ExprUtils
  ( typeIsStruct,
    mapOuterExprAnnotation,
    mapExpr,
    bindExpr,
    mapPattern,
    patternMonoid,
    mapExprDep,
  )
where

import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import Smol.Core.Types

-- helper functions for manipulating Expr types
--
typeIsStruct :: Type dep ann -> Bool
typeIsStruct TPrim {} = False
typeIsStruct TLiteral {} = False
typeIsStruct _ = True

-- | modify the outer annotation of an expression
-- useful for adding line numbers during parsing
mapOuterExprAnnotation :: (ann -> ann) -> Expr dep ann -> Expr dep ann
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

mapExpr :: (Expr dep ann -> Expr dep ann) -> Expr dep ann -> Expr dep ann
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

bindExpr :: (Applicative m) => (Expr dep ann -> m (Expr dep ann)) -> Expr dep ann -> m (Expr dep ann)
bindExpr f (EInfix ann op a b) = EInfix ann op <$> f a <*> f b
bindExpr f (EAnn ann mt expr) = EAnn ann mt <$> f expr
bindExpr _ (EPrim ann a) = pure $ EPrim ann a
bindExpr _ (EVar ann a) = pure $ EVar ann a
bindExpr _ (EConstructor ann a) = pure $ EConstructor ann a
bindExpr f (ELet ann ident expr rest) = ELet ann ident <$> f expr <*> f rest
bindExpr f (ELambda ann ident body) = ELambda ann ident <$> f body
bindExpr f (EApp ann fn arg) = EApp ann <$> f fn <*> f arg
bindExpr f (EIf ann predExp thenExp elseExp) =
  EIf ann <$> f predExp <*> f thenExp <*> f elseExp
bindExpr f (ETuple ann a as) = ETuple ann <$> f a <*> traverse f as
bindExpr _ (EGlobal ann ident) = pure $ EGlobal ann ident
bindExpr f (EGlobalLet ann ident expr rest) =
  EGlobalLet ann ident <$> f expr <*> f rest
bindExpr f (ERecord ann as) = ERecord ann <$> traverse f as
bindExpr f (ERecordAccess ann expr ident) =
  ERecordAccess ann <$> f expr <*> pure ident
bindExpr f (EPatternMatch ann patExpr pats) =
  EPatternMatch ann <$> f patExpr <*> traverse (\(a, b) -> (,) a <$> f b) pats

mapPattern :: (Pattern dep ann -> Pattern dep ann) -> Pattern dep ann -> Pattern dep ann
mapPattern _ (PLiteral ann l) = PLiteral ann l
mapPattern _ (PWildcard a) = PWildcard a
mapPattern _ (PVar ann a) = PVar ann a
mapPattern f (PTuple ann a as) = PTuple ann (f a) (f <$> as)
mapPattern f (PConstructor ann constructor as) =
  PConstructor ann constructor (f <$> as)

patternMonoid :: (Monoid a) => (Pattern dep ann -> a) -> Pattern dep ann -> a
patternMonoid _ PLiteral {} = mempty
patternMonoid _ PWildcard {} = mempty
patternMonoid _ PVar {} = mempty
patternMonoid f (PTuple _ a as) =
  f a <> foldMap f (NE.toList as)
patternMonoid f (PConstructor _ _ as) =
  foldMap f as

-- | `ParsedExpr` has module names
-- | `ResolvedExpr` has module hashes and unique ids
-- this is like NumberVars from main `mimsa`, but for now we'll bodge it
-- to get things typechecking
mapExprDep :: (forall a. depA a -> depB a) -> Expr depA ann -> Expr depB ann
mapExprDep resolve = go
  where
    go (EInfix ann op a b) = EInfix ann op (go a) (go b)
    go (EAnn ann mt expr) = EAnn ann (mapTypeDep resolve mt) (go expr)
    go (EPrim ann a) = EPrim ann a
    go (EVar ann a) =
      EVar ann (resolve a)
    go (EConstructor ann a) =
      EConstructor ann (resolve a)
    go (ELet ann ident expr rest) =
      ELet ann (resolve ident) (go expr) (go rest)
    go (ELambda ann ident body) = ELambda ann (resolve ident) (go body)
    go (EApp ann fn arg) = EApp ann (go fn) (go arg)
    go (EIf ann predExp thenExp elseExp) =
      EIf ann (go predExp) (go thenExp) (go elseExp)
    go (ETuple ann a as) = ETuple ann (go a) (go <$> as)
    go (EGlobal ann ident) = EGlobal ann ident
    go (EGlobalLet ann ident expr rest) =
      EGlobalLet ann ident (go expr) (go rest)
    go (ERecord ann as) = ERecord ann (go <$> as)
    go (ERecordAccess ann expr ident) =
      ERecordAccess ann (go expr) ident
    go (EPatternMatch ann patExpr pats) =
      EPatternMatch ann (go patExpr) (bimap (mapPatternDep resolve) go <$> pats)

mapPatternDep :: (forall a. depA a -> depB a) -> Pattern depA ann -> Pattern depB ann
mapPatternDep resolve = go
  where
    go (PLiteral ann l) = PLiteral ann l
    go (PWildcard a) = PWildcard a
    go (PVar ann a) = PVar ann (resolve a)
    go (PTuple ann a as) = PTuple ann (go a) (go <$> as)
    go (PConstructor ann constructor as) =
      PConstructor ann (resolve constructor) (go <$> as)

mapTypeDep :: (forall a. depA a -> depB a) -> Type depA ann -> Type depB ann
mapTypeDep resolve = go
  where
    go (TTuple ann a as) = TTuple ann (go a) (go <$> as)
