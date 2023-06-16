module Smol.Core.TypeUtils (mapType, monoidType) where

import Smol.Core.Types

-- helper functions for manipulating Types
mapType :: (Type dep ann -> Type dep ann) -> Type dep ann -> Type dep ann
mapType f (TFunc ann env fn arg) =
  TFunc ann (mapType f <$> env) (f fn) (f arg)
mapType f (TTuple ann tHead tTail) =
  TTuple ann (mapType f tHead) (mapType f <$> tTail)
mapType f (TInfix ann op a b) =
  TInfix ann op (mapType f a) (mapType f b)
mapType f (TArray ann i as) = TArray ann i (f as)
mapType _ (TLiteral ann l) = TLiteral ann l
mapType _ (TPrim ann p) = TPrim ann p
mapType _ (TVar ann v) = TVar ann v
mapType _ (TUnknown ann i) = TUnknown ann i
mapType f (TRecord ann parts) = TRecord ann (mapType f <$> parts)
mapType f (TApp ann fn arg) =
  TApp ann (f fn) (f arg)
mapType f (TGlobals ann globMap expr) =
  TGlobals ann (mapType f <$> globMap) (f expr)
mapType _ (TConstructor ann c) = TConstructor ann c

monoidType :: (Monoid a) => (Type dep ann -> a) -> Type dep ann -> a
monoidType _ TVar {} = mempty
monoidType _ TLiteral {} = mempty
monoidType _ TPrim {} = mempty
monoidType _ TUnknown {} = mempty
monoidType _ TConstructor {} = mempty
monoidType f (TInfix _ _ a b) = f a <> f b
monoidType f (TFunc _ closure from to) =
  foldMap f closure <> f from <> f to
monoidType f (TArray _ _ as) =
  f as
monoidType f (TTuple _ a as) =
  f a <> foldMap f as
monoidType f (TGlobals _ as expr) =
  foldMap f as <> f expr
monoidType f (TRecord _ as) =
  foldMap f as
monoidType f (TApp _ fn arg) = f fn <> f arg
