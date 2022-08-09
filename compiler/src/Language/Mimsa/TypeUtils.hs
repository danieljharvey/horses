module Language.Mimsa.TypeUtils (withMonoid, mapMonoType) where

import qualified Data.Map.Strict as M
import Language.Mimsa.Types.Typechecker

withMonoid :: (Monoid m) => (Type ann -> m) -> Type ann -> m
withMonoid _ MTVar {} = mempty
withMonoid _ MTConstructor {} = mempty
withMonoid _ MTPrim {} = mempty
withMonoid f (MTTypeApp _ a b) =
  f a <> f b
withMonoid f (MTPair _ a b) =
  f a <> f b
withMonoid f (MTArray _ as) = f as
withMonoid f (MTRecord _ as) =
  mconcat (f <$> M.elems as)
withMonoid f (MTRecordRow _ as a) =
  mconcat (f <$> M.elems as)
    <> f a
withMonoid f (MTFunction _ a b) =
  f a <> f b

mapMonoType :: (Type ann -> Type ann) -> Type ann -> Type ann
mapMonoType _ mt@MTVar {} = mt
mapMonoType _ mt@MTConstructor {} = mt
mapMonoType _ mt@MTPrim {} = mt
mapMonoType f (MTTypeApp ann a b) =
  MTTypeApp ann (f a) (f b)
mapMonoType f (MTPair ann a b) =
  MTPair ann (f a) (f b)
mapMonoType f (MTArray ann as) = MTArray ann (f as)
mapMonoType f (MTRecord ann as) =
  MTRecord ann (f <$> as)
mapMonoType f (MTRecordRow ann as a) =
  MTRecordRow ann (f <$> as) (f a)
mapMonoType f (MTFunction ann a b) =
  MTFunction ann (f a) (f b)
