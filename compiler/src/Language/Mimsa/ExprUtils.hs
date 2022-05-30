{-# LANGUAGE TupleSections #-}

module Language.Mimsa.ExprUtils
  ( withMonoid,
    mapExpr,
    bindExpr,
    toEmptyAnnotation,
    getAnnotation,
    mapPattern,
    nameFromIdent,
    detailsFromIdent,
  )
where

import Data.Bifunctor (second)
import qualified Data.Map as M
import Language.Mimsa.Types.AST.Expr (Expr (..))
import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Types.AST.Pattern

-------
-- Functions for operating on the Expr type
-------

-- | Removes any annotations in the expression, useful when serialising
-- expressions
toEmptyAnnotation ::
  (Monoid b) =>
  Expr var a ->
  Expr var b
toEmptyAnnotation = fmap (const mempty)

-- | Retrieve the annotation for any given Expression
getAnnotation :: Expr var ann -> ann
getAnnotation (MyLiteral ann _) = ann
getAnnotation (MyAnnotation ann _ _) = ann
getAnnotation (MyVar ann _ _) = ann
getAnnotation (MyLet ann _ _ _) = ann
getAnnotation (MyLetPattern ann _ _ _) = ann
getAnnotation (MyInfix ann _ _ _) = ann
getAnnotation (MyLambda ann _ _) = ann
getAnnotation (MyApp ann _ _) = ann
getAnnotation (MyIf ann _ _ _) = ann
getAnnotation (MyPair ann _ _) = ann
getAnnotation (MyRecord ann _) = ann
getAnnotation (MyRecordAccess ann _ _) = ann
getAnnotation (MyData ann _ _) = ann
getAnnotation (MyConstructor ann _ _) = ann
getAnnotation (MyTypedHole ann _) = ann
getAnnotation (MyDefineInfix ann _ _ _) = ann
getAnnotation (MyArray ann _) = ann
getAnnotation (MyPatternMatch ann _ _) = ann

-- | Given a function `f` that turns any piece of the expression in a Monoid
-- `m`, flatten the entire expression into `m`
withMonoid ::
  (Monoid m) =>
  (Expr var ann -> (Bool, m)) ->
  Expr var ann ->
  m
withMonoid f whole@(MyLiteral _ _) = snd (f whole)
withMonoid f whole@MyVar {} = snd (f whole)
withMonoid f whole@(MyAnnotation _ _ expr) =
  let (go, m) = f whole
   in if not go
        then m
        else m <> withMonoid f expr
withMonoid f whole@(MyLet _ _ bindExpr' inExpr) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f bindExpr'
            <> withMonoid f inExpr
withMonoid f whole@(MyLetPattern _ _ expr body) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f expr
            <> withMonoid f body
withMonoid f whole@(MyInfix _ _ a b) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f a
            <> withMonoid f b
withMonoid f whole@(MyLambda _ _binder expr) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f expr
withMonoid f whole@(MyApp _ func arg) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f func
            <> withMonoid f arg
withMonoid f whole@(MyIf _ matchExpr thenExpr elseExpr) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f matchExpr
            <> withMonoid f thenExpr
            <> withMonoid f elseExpr
withMonoid f whole@(MyPair _ a b) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f a
            <> withMonoid f b
withMonoid f whole@(MyRecord _ items) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> mconcat
              ( snd <$> M.toList (withMonoid f <$> items)
              )
withMonoid f whole@(MyArray _ items) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> mconcat
              (withMonoid f <$> items)
withMonoid f whole@(MyRecordAccess _ expr _name) =
  let (go, m) = f whole
   in if not go then m else m <> withMonoid f expr
withMonoid f whole@(MyData _ _ expr) =
  let (go, m) = f whole
   in if not go then m else m <> withMonoid f expr
withMonoid f whole@MyConstructor {} = snd (f whole)
withMonoid f whole@MyTypedHole {} = snd (f whole)
withMonoid f whole@(MyDefineInfix _ _ infixExpr inExpr) =
  let (go, m) = f whole
   in if not go
        then m
        else m <> withMonoid f infixExpr <> withMonoid f inExpr
withMonoid f whole@(MyPatternMatch _ matchExpr matches) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m
            <> withMonoid f matchExpr
            <> mconcat
              (withMonoid f <$> (snd <$> matches))

-- | Map a function `f` over the expression. This function takes care of
-- recursing through the Expression
mapExpr :: (Expr a b -> Expr a b) -> Expr a b -> Expr a b
mapExpr _ (MyLiteral ann a) = MyLiteral ann a
mapExpr _ (MyVar ann modName a) = MyVar ann modName a
mapExpr f (MyAnnotation ann mt expr) =
  MyAnnotation ann mt (f expr)
mapExpr f (MyLet ann binder bindExpr' inExpr) =
  MyLet ann binder (f bindExpr') (f inExpr)
mapExpr f (MyLetPattern ann pat expr body) =
  MyLetPattern ann pat (f expr) (f body)
mapExpr f (MyInfix ann op a b) = MyInfix ann op (f a) (f b)
mapExpr f (MyLambda ann binder expr) = MyLambda ann binder (f expr)
mapExpr f (MyApp ann func arg) = MyApp ann (f func) (f arg)
mapExpr f (MyIf ann matchExpr thenExpr elseExpr) =
  MyIf ann (f matchExpr) (f thenExpr) (f elseExpr)
mapExpr f (MyPair ann a b) = MyPair ann (f a) (f b)
mapExpr f (MyRecord ann items) = MyRecord ann (f <$> items)
mapExpr f (MyRecordAccess ann expr name) =
  MyRecordAccess ann (f expr) name
mapExpr f (MyArray ann items) = MyArray ann (f <$> items)
mapExpr f (MyData ann dt expr) = MyData ann dt (f expr)
mapExpr _ (MyConstructor ann modName cons) = MyConstructor ann modName cons
mapExpr f (MyPatternMatch ann matchExpr patterns) =
  MyPatternMatch ann (f matchExpr) (second f <$> patterns)
mapExpr _ (MyTypedHole ann a) = MyTypedHole ann a
mapExpr f (MyDefineInfix ann op infixExpr inExpr) =
  MyDefineInfix ann op (f infixExpr) (f inExpr)

-- | Bind a function `f` over the expression. This function takes care of
-- recursing through the expression.
bindExpr ::
  (Applicative m) =>
  (Expr a b -> m (Expr a b)) ->
  Expr a b ->
  m (Expr a b)
bindExpr _ (MyLiteral ann a) =
  pure $ MyLiteral ann a
bindExpr _ (MyVar ann modName a) =
  pure $ MyVar ann modName a
bindExpr f (MyAnnotation ann mt expr) =
  MyAnnotation ann mt <$> f expr
bindExpr f (MyLet ann binder bindExpr' inExpr) =
  MyLet ann binder <$> f bindExpr' <*> f inExpr
bindExpr f (MyLetPattern ann pat expr body) =
  MyLetPattern ann pat <$> f expr <*> f body
bindExpr f (MyInfix ann op a b) =
  MyInfix ann op <$> f a <*> f b
bindExpr f (MyLambda ann binder expr) =
  MyLambda ann binder <$> f expr
bindExpr f (MyApp ann func arg) =
  MyApp ann <$> f func <*> f arg
bindExpr f (MyIf ann matchExpr thenExpr elseExpr) =
  MyIf ann <$> f matchExpr <*> f thenExpr <*> f elseExpr
bindExpr f (MyPair ann a b) =
  MyPair ann <$> f a <*> f b
bindExpr f (MyRecord ann items) =
  MyRecord ann <$> traverse f items
bindExpr f (MyRecordAccess ann expr name) =
  MyRecordAccess ann <$> f expr <*> pure name
bindExpr f (MyArray ann items) =
  MyArray ann <$> traverse f items
bindExpr f (MyData ann dt expr) =
  MyData ann dt <$> f expr
bindExpr _ (MyConstructor ann modName cons) =
  pure $ MyConstructor ann modName cons
bindExpr _ (MyTypedHole ann a) = pure (MyTypedHole ann a)
bindExpr f (MyDefineInfix ann op infixExpr expr) =
  MyDefineInfix ann op <$> f infixExpr <*> f expr
bindExpr f (MyPatternMatch ann matchExpr patterns) =
  MyPatternMatch
    ann
    <$> f matchExpr
    <*> traverse traverseSecond patterns
  where
    traverseSecond (a, b) = (a,) <$> f b

-- | Map a function `f` over the pattern. This function takes care of
-- recursing through the Pattern
mapPattern :: (Pattern a b -> Pattern a b) -> Pattern a b -> Pattern a b
mapPattern _ (PWildcard ann) = PWildcard ann
mapPattern _ (PVar ann a) = PVar ann a
mapPattern _ (PLit ann a) = PLit ann a
mapPattern f (PConstructor ann modName tyCon vars) =
  PConstructor ann modName tyCon (f <$> vars)
mapPattern f (PPair ann a b) =
  PPair ann (f a) (f b)
mapPattern f (PRecord ann as) =
  PRecord ann (f <$> as)
mapPattern f (PArray ann as spread) =
  PArray ann (f <$> as) spread
mapPattern _ (PString ann pHead pTail) =
  PString ann pHead pTail

nameFromIdent :: Identifier var ann -> var
nameFromIdent = fst . detailsFromIdent

detailsFromIdent :: Identifier var ann -> (var, ann)
detailsFromIdent (Identifier ann name) = (name, ann)
