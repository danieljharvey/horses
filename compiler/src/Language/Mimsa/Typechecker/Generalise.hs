module Language.Mimsa.Typechecker.Generalise
  ( generalise,
    freeTypeVars,
  )
where

import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

freeTypeVars :: MonoType -> [TypeIdentifier]
freeTypeVars (MTVar _ b) = [b]
freeTypeVars (MTFunction _ t1 t2) = freeTypeVars t1 <> freeTypeVars t2
freeTypeVars (MTPair _ a b) = freeTypeVars a <> freeTypeVars b
freeTypeVars (MTRecord _ as) = mconcat (freeTypeVars . snd <$> M.toList as)
freeTypeVars (MTRecordRow _ as rest) =
  mconcat (freeTypeVars . snd <$> M.toList as)
    <> freeTypeVars rest
freeTypeVars (MTArray _ a) = freeTypeVars a
freeTypeVars (MTPrim _ _) = mempty
freeTypeVars (MTConstructor _ _) = mempty
freeTypeVars (MTTypeApp _ a b) = freeTypeVars a <> freeTypeVars b

freeTypeVarsScheme :: Scheme -> [TypeIdentifier]
freeTypeVarsScheme (Scheme vars t) =
  freeTypeVars t \\ vars

freeTypeVarsCtx :: Environment -> [TypeIdentifier]
freeTypeVarsCtx (Environment env _ _ _) =
  foldMap freeTypeVarsScheme (M.elems env)

generalise :: Environment -> MonoType -> Scheme
generalise env ty =
  Scheme (S.toList $ S.fromList vars) ty
  where
    vars =
      freeTypeVars ty \\ freeTypeVarsCtx env
