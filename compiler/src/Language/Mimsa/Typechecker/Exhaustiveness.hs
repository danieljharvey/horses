module Language.Mimsa.Typechecker.Exhaustiveness where

import Data.Functor
import Debug.Trace
import Language.Mimsa.Types.AST

-- | given a list of patterns, return a list of missing patterns
isExhaustive ::
  (Show ann, Show var, Eq var, Monoid ann) =>
  [Pattern var ann] ->
  [Pattern var ann]
isExhaustive patterns =
  let generated = mconcat (generateRequired <$> patterns)
   in filterMissing patterns generated

-- | Given a pattern, generate others required for it
generateRequired :: (Monoid ann) => Pattern var ann -> [Pattern var ann]
generateRequired (PLit _ (MyBool True)) = [PLit mempty (MyBool False)]
generateRequired (PLit _ (MyBool False)) = [PLit mempty (MyBool True)]
generateRequired (PLit _ (MyInt _)) = [PWildcard mempty]
generateRequired (PLit _ (MyString _)) = [PWildcard mempty]
generateRequired (PPair _ l r) = do
  ls <- generateRequired l
  rs <- generateRequired r
  pure (PPair mempty ls rs)
generateRequired (PRecord _ a) = do
  as <- traverse generateRequired a
  pure (PRecord mempty as)
generateRequired _ = mempty

-- filter outstanding items
filterMissing ::
  (Show var, Show ann, Eq var) =>
  [Pattern var ann] ->
  [Pattern var ann] ->
  [Pattern var ann]
filterMissing patterns required =
  foldr (\a b -> annihiliatePattern (traceShowId a) (traceShowId b)) required patterns
  where
    annihiliatePattern pat remaining =
      filter
        ( annihilate
            (removeAnn pat)
            . removeAnn
        )
        remaining

removeAnn :: Pattern var ann -> Pattern var ()
removeAnn p = p $> ()

-- does left pattern satisfy right pattern?
annihilate :: (Eq var) => Pattern var () -> Pattern var () -> Bool

-- | if left is on the right, get rid
annihilate a b | a == b = False
annihilate (PWildcard _) _ = False
annihilate (PVar _ _) _ = False
annihilate (PPair _ a b) (PPair _ a' b') =
  annihilate a a' || annihilate b b'
annihilate _ _as = True
