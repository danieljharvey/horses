module Language.Mimsa.Typechecker.Generalise where

import Data.List (nub, (\\))
import qualified Data.Map as M
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

freeVars :: MonoType -> [TypeIdentifier]
freeVars (MTVar _ b) = [b]
freeVars (MTFunction _ t1 t2) = freeVars t1 <> freeVars t2
freeVars (MTPair _ a b) = freeVars a <> freeVars b
freeVars (MTRecord _ as) = mconcat (freeVars . snd <$> M.toList as)
freeVars (MTArray _ a) = freeVars a
freeVars (MTPrim _ _) = mempty
freeVars MTData {} = mempty

generalise :: Substitutions -> MonoType -> Scheme
generalise (Substitutions subst) ty = Scheme free ty
  where
    free = nub $ freeVars ty \\ map fst (M.toList subst)
