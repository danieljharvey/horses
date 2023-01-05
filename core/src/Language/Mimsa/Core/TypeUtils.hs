module Language.Mimsa.Core.TypeUtils (dataTypeWithVars, withMonoidType, mapType, bindType) where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Language.Mimsa.Core.Types.Identifiers
import Language.Mimsa.Core.Types.Module
import Language.Mimsa.Core.Types.Type

dataTypeWithVars ::
  (Monoid ann) =>
  ann ->
  Maybe ModuleName ->
  TypeName ->
  [Type ann] ->
  Type ann
dataTypeWithVars ann modName tyName =
  foldl'
    (MTTypeApp mempty)
    (MTConstructor ann modName tyName)

withMonoidType :: (Monoid m) => (Type ann -> m) -> Type ann -> m
withMonoidType _ MTVar {} = mempty
withMonoidType _ MTConstructor {} = mempty
withMonoidType _ MTPrim {} = mempty
withMonoidType f (MTTypeApp _ a b) =
  f a <> f b
withMonoidType f (MTTuple _ a as) =
  f a <> foldMap f as
withMonoidType f (MTArray _ as) = f as
withMonoidType f (MTRecord _ as Nothing) =
  mconcat (f <$> M.elems as)
withMonoidType f (MTRecord _ as (Just a)) =
  mconcat (f <$> M.elems as)
    <> f a
withMonoidType f (MTFunction _ a b) =
  f a <> f b
withMonoidType f (MTGlobals _ globs rest expr) =
  mconcat (f <$> M.elems globs) <> maybe mempty f rest <> f expr

mapType :: (Type ann -> Type ann) -> Type ann -> Type ann
mapType _ mt@MTVar {} = mt
mapType _ mt@MTConstructor {} = mt
mapType _ mt@MTPrim {} = mt
mapType f (MTTypeApp ann a b) =
  MTTypeApp ann (f a) (f b)
mapType f (MTTuple ann a as) =
  MTTuple ann (f a) (f <$> as)
mapType f (MTArray ann as) = MTArray ann (f as)
mapType f (MTRecord ann as a) =
  MTRecord ann (f <$> as) (f <$> a)
mapType f (MTFunction ann a b) =
  MTFunction ann (f a) (f b)
mapType f (MTGlobals ann globs rest expr) =
  MTGlobals ann (f <$> globs) (f <$> rest) (f expr)

-- lift a monadic action over a type
bindType ::
  (Applicative m) =>
  (Type ann -> m (Type ann)) ->
  Type ann ->
  m (Type ann)
bindType f mt = case mt of
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
