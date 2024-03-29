{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Smol.Core.ExprUtils
  ( typeIsStruct,
    mapOuterExprAnnotation,
    mapExpr,
    bindExpr,
    mapPattern,
    patternMonoid,
    mapExprDep,
    mapTypeDep,
    mapDataTypeDep,
    withMonoid,
  )
where

import Data.Bifunctor
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Smol.Core.Types

-- helper functions for manipulating Expr types
--
typeIsStruct :: Type dep ann -> Bool
typeIsStruct TPrim {} = False
typeIsStruct TLiteral {} = False
typeIsStruct TUnknown {} = False
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
    EArray ann a -> EArray (f ann) a
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
mapExpr f (EArray ann as) = EArray ann (f <$> as)
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
bindExpr f (EArray ann as) = EArray ann <$> traverse f as
bindExpr f (ERecord ann as) = ERecord ann <$> traverse f as
bindExpr f (ERecordAccess ann expr ident) =
  ERecordAccess ann <$> f expr <*> pure ident
bindExpr f (EPatternMatch ann patExpr pats) =
  EPatternMatch ann <$> f patExpr <*> traverse (\(a, b) -> (,) a <$> f b) pats

mapPattern :: (Pattern dep ann -> Pattern dep ann) -> Pattern dep ann -> Pattern dep ann
mapPattern _ (PLiteral ann l) = PLiteral ann l
mapPattern _ (PWildcard a) = PWildcard a
mapPattern f (PArray ann as spread) = PArray ann (f <$> as) spread -- do we need to map spread somehow
mapPattern _ (PVar ann a) = PVar ann a
mapPattern f (PTuple ann a as) = PTuple ann (f a) (f <$> as)
mapPattern f (PConstructor ann constructor as) =
  PConstructor ann constructor (f <$> as)

patternMonoid :: (Monoid a) => (Pattern dep ann -> a) -> Pattern dep ann -> a
patternMonoid _ PLiteral {} = mempty
patternMonoid _ PWildcard {} = mempty
patternMonoid _ PVar {} = mempty
patternMonoid f (PArray _ as _) = foldMap f as
patternMonoid f (PTuple _ a as) =
  f a <> foldMap f (NE.toList as)
patternMonoid f (PConstructor _ _ as) =
  foldMap f as

-- | `ParsedExpr` has module names
-- | `ResolvedExpr` has module hashes and unique ids
-- this is like NumberVars from main `mimsa`, but for now we'll bodge it
-- to get things typechecking
mapExprDep ::
  (Ord (depB Identifier)) =>
  (forall a. depA a -> depB a) ->
  Expr depA ann ->
  Expr depB ann
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
    go (EArray ann as) = EArray ann (go <$> as)
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
    go (PArray ann as spread) = PArray ann (go <$> as) (mapSpreadDep resolve spread)
    go (PConstructor ann constructor as) =
      PConstructor ann (resolve constructor) (go <$> as)

mapSpreadDep :: (forall a. depA a -> depB a) -> Spread depA ann -> Spread depB ann
mapSpreadDep resolve = go
  where
    go NoSpread = NoSpread
    go (SpreadWildcard ann) = SpreadWildcard ann
    go (SpreadValue ann a) = SpreadValue ann (resolve a)

mapTypeDep :: (Ord (depB Identifier)) => (forall a. depA a -> depB a) -> Type depA ann -> Type depB ann
mapTypeDep resolve = go
  where
    go (TVar ann v) = TVar ann (resolve v)
    go (TInfix ann op a b) = TInfix ann op (go a) (go b)
    go (TTuple ann a as) = TTuple ann (go a) (go <$> as)
    go (TArray ann i as) = TArray ann i (go as)
    go (TLiteral ann a) = TLiteral ann a
    go (TPrim ann p) = TPrim ann p
    go (TFunc ann env a b) = TFunc ann (M.mapKeys resolve $ go <$> env) (go a) (go b)
    go (TUnknown ann i) = TUnknown ann i
    go (TRecord ann as) = TRecord ann (go <$> as)
    go (TApp ann a b) = TApp ann (go a) (go b)
    go (TConstructor ann constructor) = TConstructor ann (resolve constructor)

mapDataTypeDep ::
  (Ord (depB Identifier)) =>
  (forall a. depA a -> depB a) ->
  DataType depA ann ->
  DataType depB ann
mapDataTypeDep resolve (DataType {dtName, dtVars, dtConstructors}) =
  let newConstructors = (fmap . fmap) (mapTypeDep resolve) dtConstructors
   in DataType {dtName, dtVars, dtConstructors = newConstructors}

-- | Given a function `f` that turns any piece of the expression in a Monoid
-- `m`, flatten the entire expression into `m`
withMonoid ::
  (Monoid m) =>
  (Expr var ann -> (Bool, m)) ->
  Expr var ann ->
  m
withMonoid f whole@(EPrim _ _) = snd (f whole)
withMonoid f whole@EVar {} = snd (f whole)
withMonoid f whole@(EAnn _ _ expr) =
  let (go, m) = f whole
   in if not go
        then m
        else m <> withMonoid f expr
withMonoid f whole@(ELet _ _ bindExpr' inExpr) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f bindExpr'
            <> withMonoid f inExpr
withMonoid f whole@(EInfix _ _ a b) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f a
            <> withMonoid f b
withMonoid f whole@(ELambda _ _binder expr) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f expr
withMonoid f whole@(EApp _ func arg) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f func
            <> withMonoid f arg
withMonoid f whole@(EIf _ matchExpr thenExpr elseExpr) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f matchExpr
            <> withMonoid f thenExpr
            <> withMonoid f elseExpr
withMonoid f whole@(ETuple _ a as) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f a
            <> mconcat (withMonoid f <$> NE.toList as)
withMonoid f whole@(ERecord _ items) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> mconcat
              ( snd <$> M.toList (withMonoid f <$> items)
              )
withMonoid f whole@(EArray _ items) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> mconcat
              (withMonoid f <$> toList items)
withMonoid f whole@(ERecordAccess _ expr _name) =
  let (go, m) = f whole
   in if not go then m else m <> withMonoid f expr
withMonoid f whole@EConstructor {} = snd (f whole)
withMonoid f whole@(EPatternMatch _ matchExpr matches) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f matchExpr
            <> mconcat
              (withMonoid f <$> (snd <$> NE.toList matches))
