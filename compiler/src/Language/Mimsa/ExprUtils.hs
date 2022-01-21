{-# LANGUAGE TupleSections #-}

module Language.Mimsa.ExprUtils
  ( withMonoid,
    mapExpr,
    bindExpr,
    toEmptyAnnotation,
    getAnnotation,
    mapOuterExprAnnotation,
  )
where

import Data.Bifunctor (second)
import qualified Data.Map as M
import Language.Mimsa.Types.AST.Expr (Expr (..))

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
getAnnotation (MyVar ann _) = ann
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
getAnnotation (MyConstructor ann _) = ann
getAnnotation (MyTypedHole ann _) = ann
getAnnotation (MyDefineInfix ann _ _ _) = ann
getAnnotation (MyArray ann _) = ann
getAnnotation (MyPatternMatch ann _ _) = ann
getAnnotation (MyFromContext ann _) = ann

-- | Given a function `f` that turns any piece of the expression in a Monoid
-- `m`, flatten the entire expression into `m`
withMonoid ::
  (Monoid m) =>
  (Expr var ann -> (Bool, m)) ->
  Expr var ann ->
  m
withMonoid f whole@(MyLiteral _ _) = snd (f whole)
withMonoid f whole@(MyVar _ _) = snd (f whole)
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
          m <> withMonoid f expr
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
          m <> withMonoid f func
            <> withMonoid f arg
withMonoid f whole@(MyIf _ matchExpr thenExpr elseExpr) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m <> withMonoid f matchExpr
            <> withMonoid f thenExpr
            <> withMonoid f elseExpr
withMonoid f whole@(MyPair _ a b) =
  let (go, m) = f whole
   in if not go
        then m
        else
          m <> withMonoid f a
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
withMonoid f whole@(MyConstructor _ _) = snd (f whole)
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
          m <> withMonoid f matchExpr
            <> mconcat
              (withMonoid f <$> (snd <$> matches))
withMonoid f whole@MyFromContext {} = snd (f whole)

-- | Map a function `f` over the expression. This function takes care of
-- recursing through the Expression
mapExpr :: (Expr a b -> Expr a b) -> Expr a b -> Expr a b
mapExpr _ (MyLiteral ann a) = MyLiteral ann a
mapExpr _ (MyVar ann a) = MyVar ann a
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
mapExpr _ (MyConstructor ann cons) = MyConstructor ann cons
mapExpr f (MyPatternMatch ann matchExpr patterns) =
  MyPatternMatch ann (f matchExpr) (second f <$> patterns)
mapExpr _ (MyTypedHole ann a) = MyTypedHole ann a
mapExpr f (MyDefineInfix ann op infixExpr inExpr) =
  MyDefineInfix ann op (f infixExpr) (f inExpr)
mapExpr _ (MyFromContext ann name) = MyFromContext ann name

-- | Bind a function `f` over the expression. This function takes care of
-- recursing through the expression.
bindExpr ::
  (Applicative m) =>
  (Expr a b -> m (Expr a b)) ->
  Expr a b ->
  m (Expr a b)
bindExpr _ (MyLiteral ann a) =
  pure $ MyLiteral ann a
bindExpr _ (MyVar ann a) =
  pure $ MyVar ann a
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
bindExpr _ (MyConstructor ann cons) =
  pure $ MyConstructor ann cons
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
bindExpr _ (MyFromContext ann name) =
  pure $ MyFromContext ann name

-- | modify the outer annotation of an expression
-- useful for adding line numbers during parsing
mapOuterExprAnnotation :: (ann -> ann) -> Expr a ann -> Expr a ann
mapOuterExprAnnotation f expr' =
  case expr' of
    MyInfix ann a op b -> MyInfix (f ann) a op b
    MyLiteral ann a -> MyLiteral (f ann) a
    MyVar ann a -> MyVar (f ann) a
    MyLet ann a b c -> MyLet (f ann) a b c
    MyLetPattern ann a b c -> MyLetPattern (f ann) a b c
    MyLambda ann a b -> MyLambda (f ann) a b
    MyApp ann a b -> MyApp (f ann) a b
    MyIf ann a b c -> MyIf (f ann) a b c
    MyPair ann a b -> MyPair (f ann) a b
    MyRecord ann as -> MyRecord (f ann) as
    MyRecordAccess ann a b -> MyRecordAccess (f ann) a b
    MyArray ann as -> MyArray (f ann) as
    MyData ann a b -> MyData (f ann) a b
    MyConstructor ann a -> MyConstructor (f ann) a
    MyTypedHole ann a -> MyTypedHole (f ann) a
    MyDefineInfix ann a b c -> MyDefineInfix (f ann) a b c
    MyPatternMatch ann a b -> MyPatternMatch (f ann) a b
    MyFromContext ann name -> MyFromContext (f ann) name
