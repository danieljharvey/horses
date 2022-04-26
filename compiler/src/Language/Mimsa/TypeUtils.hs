module Language.Mimsa.TypeUtils (withMonoid) where

import qualified Data.Map as M
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
