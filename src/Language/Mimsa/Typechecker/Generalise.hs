module Language.Mimsa.Typechecker.Generalise where

import Data.List
import qualified Data.Map as M
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Scheme
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Variable

freeVars :: MonoType -> [Variable]
freeVars (MTVar b) = [b]
freeVars (MTFunction t1 t2) = freeVars t1 <> freeVars t2
freeVars (MTPair a b) = freeVars a <> freeVars b
freeVars (MTSum a b) = freeVars a <> freeVars b
freeVars (MTList a) = freeVars a
freeVars (MTRecord as) = mconcat (freeVars . snd <$> (M.toList as))
freeVars MTInt = mempty
freeVars MTString = mempty
freeVars MTBool = mempty
freeVars MTUnit = mempty

generalise :: Substitutions -> MonoType -> Scheme
generalise (Substitutions subst) ty = Scheme free ty
  where
    free = nub $ freeVars ty \\ map fst (M.toList subst)
