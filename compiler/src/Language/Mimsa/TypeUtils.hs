module Language.Mimsa.TypeUtils (withMonoid, mapMonoType, bindMonoType) where

import qualified Data.Map.Strict as M
import Language.Mimsa.Types.Typechecker

withMonoid :: (Monoid m) => (Type ann -> m) -> Type ann -> m
withMonoid _ MTVar {} = mempty
withMonoid _ MTConstructor {} = mempty
withMonoid _ MTPrim {} = mempty
withMonoid f (MTTypeApp _ a b) =
  f a <> f b
withMonoid f (MTTuple _ a as) =
  f a <> foldMap f as
withMonoid f (MTArray _ as) = f as
withMonoid f (MTRecord _ as Nothing) =
  mconcat (f <$> M.elems as)
withMonoid f (MTRecord _ as (Just a)) =
  mconcat (f <$> M.elems as)
    <> f a
withMonoid f (MTFunction _ a b) =
  f a <> f b
withMonoid f (MTGlobals _ globs rest expr) =
  mconcat (f <$> M.elems globs) <> maybe mempty f rest <> f expr

mapMonoType :: (Type ann -> Type ann) -> Type ann -> Type ann
mapMonoType _ mt@MTVar {} = mt
mapMonoType _ mt@MTConstructor {} = mt
mapMonoType _ mt@MTPrim {} = mt
mapMonoType f (MTTypeApp ann a b) =
  MTTypeApp ann (f a) (f b)
mapMonoType f (MTTuple ann a as) =
  MTTuple ann (f a) (f <$> as)
mapMonoType f (MTArray ann as) = MTArray ann (f as)
mapMonoType f (MTRecord ann as a) =
  MTRecord ann (f <$> as) (f <$> a)
mapMonoType f (MTFunction ann a b) =
  MTFunction ann (f a) (f b)
mapMonoType f (MTGlobals ann globs rest expr) =
  MTGlobals ann (f <$> globs) (f <$> rest) (f expr)

-- lift a monadic action over a type
bindMonoType ::
  (Applicative m) =>
  (Type ann -> m (Type ann)) ->
  Type ann ->
  m (Type ann)
bindMonoType f mt = case mt of
  MTVar ann tyIdent -> pure (MTVar ann tyIdent)
  MTPrim ann a -> pure (MTPrim ann a)
  MTFunction ann arg fun ->
    MTFunction ann
      <$> f arg
      <*> f fun
  MTTuple ann a as ->
    MTTuple ann
      <$> f a
      <*> traverse f as
  MTRecord ann as rest ->
    MTRecord ann
      <$> traverse f as
      <*> traverse f rest
  MTArray ann a -> MTArray ann <$> f a
  MTConstructor ann modName name ->
    pure (MTConstructor ann modName name)
  MTTypeApp ann func arg ->
    MTTypeApp ann <$> f func <*> f arg
  MTGlobals ann globs rest expr ->
    MTGlobals ann
      <$> traverse f globs
      <*> traverse f rest
      <*> f expr
