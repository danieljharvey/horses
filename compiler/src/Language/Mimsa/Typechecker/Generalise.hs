module Language.Mimsa.Typechecker.Generalise
  ( generalise,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

removeLambdas :: MonoType -> [TypeIdentifier]
removeLambdas (MTVar _ b) = [b]
removeLambdas (MTFunction _ _t1 t2) = removeLambdas t2
removeLambdas (MTPair _ a b) = removeLambdas a <> removeLambdas b
removeLambdas (MTRecord _ as) = mconcat (removeLambdas . snd <$> M.toList as)
removeLambdas (MTRecordRow _ as rest) =
  mconcat (removeLambdas . snd <$> M.toList as)
    <> removeLambdas rest
removeLambdas (MTArray _ a) = removeLambdas a
removeLambdas (MTPrim _ _) = mempty
removeLambdas MTData {} = mempty

generalise :: MonoType -> Scheme
generalise ty =
  Scheme (removeLambdas ty) ty
