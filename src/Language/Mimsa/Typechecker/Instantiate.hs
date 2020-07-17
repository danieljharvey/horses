module Language.Mimsa.Typechecker.Instantiate (instantiate) where

import qualified Data.Map as M
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types

-- get a type from scope, having first replaced all the bound vars with free
-- ones
instantiate :: MonoType -> TcMonad MonoType
instantiate mt = snd <$> freeTheVars mempty mt

freeTheVars :: Substitutions -> MonoType -> TcMonad (Substitutions, MonoType)
freeTheVars subst ty = case ty of
  MTVar (TVFree i) -> pure (subst, MTVar (TVFree i))
  MTVar (TVBound i) -> do
    case M.lookup i (getSubstitutions subst) of
      Just mt -> pure (subst, mt)
      Nothing -> do
        tyNew <- getUnknown
        let newSubs = Substitutions $ M.singleton i tyNew
        pure (newSubs <> subst, tyNew)
  MTFunction arg res -> do
    (s1, tyArg) <- freeTheVars subst arg
    (s2, tyRes) <- freeTheVars (s1 <> subst) res
    pure (s2 <> s1 <> subst, MTFunction tyArg tyRes)
  MTPair a b -> do
    (s1, tyA) <- freeTheVars subst a
    (s2, tyB) <- freeTheVars (s1 <> subst) b
    pure (s2 <> s1 <> subst, MTPair tyA tyB)
  MTList a -> do
    (s1, tyA) <- freeTheVars subst a
    pure (s1 <> subst, MTList tyA)
  MTRecord a -> do
    (subs, typesMap) <-
      foldr
        ( \(key, var) fromLast -> do
            (newSubs, newMap) <- fromLast
            (s1, tyVar) <- freeTheVars (newSubs <> subst) var
            pure (s1 <> newSubs, newMap <> M.singleton key tyVar)
        )
        ( pure
            (mempty, mempty)
        )
        (M.toList a)
    pure (subs, MTRecord typesMap)
  MTSum a b -> do
    (s1, tyL) <- freeTheVars subst a
    (s2, tyR) <- freeTheVars (s1 <> subst) b
    pure (s2 <> s1 <> subst, MTSum tyL tyR)
  MTInt -> pure (subst, MTInt)
  MTString -> pure (subst, MTString)
  MTBool -> pure (subst, MTBool)
  MTUnit -> pure (subst, MTUnit)
