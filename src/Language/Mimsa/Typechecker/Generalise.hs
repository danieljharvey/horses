module Language.Mimsa.Typechecker.Generalise where

import Data.List
import qualified Data.Map as M
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Scheme
import Language.Mimsa.Types.Substitutions

freeVars :: MonoType -> [Variable]
freeVars (MTVar b) = [b]
freeVars (MTFunction t1 t2) = freeVars t1 <> freeVars t2
freeVars (MTPair a b) = freeVars a <> freeVars b
freeVars (MTRecord as) = mconcat (freeVars . snd <$> M.toList as)
freeVars (MTPrim _) = mempty
freeVars (MTData _ _) = mempty

generalise :: Substitutions -> MonoType -> Scheme
generalise (Substitutions subst) ty = Scheme free ty
  where
    free = nub $ freeVars ty \\ map fst (M.toList subst)
