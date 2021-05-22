{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.Exhaustiveness
  ( isExhaustive,
    redundantCases,
    validatePatterns,
  )
where

import Control.Monad.Except
import Data.Foldable
import Data.Functor
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker.Environment

validatePatterns ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  [Pattern Variable Annotation] ->
  m ()
validatePatterns env ann patterns = do
  missing <- isExhaustive env patterns
  _ <- case missing of
    [] -> pure ()
    _ -> throwError (PatternMatchErr (MissingPatterns ann missing))
  redundant <- redundantCases env patterns
  case redundant of
    [] -> pure ()
    _ -> throwError (PatternMatchErr (RedundantPatterns ann redundant))

-- | given a list of patterns, return a list of missing patterns
isExhaustive ::
  (Eq var, MonadError TypeError m) =>
  Environment ->
  [Pattern var Annotation] ->
  m [Pattern var Annotation]
isExhaustive env patterns = do
  generated <-
    mconcat
      <$> traverse (generate env) patterns
  pure $ filterMissing patterns generated

generate ::
  (MonadError TypeError m) =>
  Environment ->
  Pattern var Annotation ->
  m [Pattern var Annotation]
generate env pat = (<>) [pat] <$> generateRequired env pat

-- | Given a pattern, generate others required for it
generateRequired ::
  (MonadError TypeError m) =>
  Environment ->
  Pattern var Annotation ->
  m [Pattern var Annotation]
generateRequired _ (PLit _ (MyBool True)) = pure [PLit mempty (MyBool False)]
generateRequired _ (PLit _ (MyBool False)) = pure [PLit mempty (MyBool True)]
generateRequired _ (PLit _ (MyInt _)) = pure [PWildcard mempty]
generateRequired _ (PLit _ (MyString _)) = pure [PWildcard mempty]
generateRequired env (PPair _ l r) = do
  ls <- generateRequired env l
  rs <- generateRequired env r
  let allPairs = PPair mempty <$> ls <*> rs
  pure allPairs
generateRequired env (PRecord _ items) = do
  items' <- traverse (generateRequired env) items
  pure (PRecord mempty <$> sequence items')
generateRequired env (PConstructor ann tyCon args) = do
  dt <- lookupConstructor env ann tyCon
  newFromArgs <- traverse (generateRequired env) args
  newDataTypes <- requiredFromDataType dt
  let newCons = PConstructor mempty tyCon <$> sequence newFromArgs
  pure (newCons <> newDataTypes)
generateRequired _ _ = pure mempty

requiredFromDataType ::
  (MonadError TypeError m) =>
  DataType Annotation ->
  m [Pattern var Annotation]
requiredFromDataType (DataType _ _ cons) = do
  let new (n, as) =
        [ PConstructor
            mempty
            n
            (PWildcard mempty <$ as)
        ]
  pure $ mconcat (new <$> M.toList cons)

-- filter outstanding items
filterMissing ::
  (Eq var, Eq ann) =>
  [Pattern var ann] ->
  [Pattern var ann] ->
  [Pattern var ann]
filterMissing patterns required =
  nub $ foldr annihiliatePattern required patterns
  where
    annihiliatePattern pat remaining =
      filter
        ( not
            . annihilate
              (removeAnn pat)
            . removeAnn
        )
        remaining

removeAnn :: Pattern var ann -> Pattern var ()
removeAnn p = p $> ()

-- does left pattern satisfy right pattern?
annihilate :: (Eq var) => Pattern var () -> Pattern var () -> Bool

-- | if left is on the right, get rid
annihilate a b | a == b = True
annihilate (PWildcard _) _ = True
annihilate (PVar _ _) _ = True
annihilate (PPair _ a b) (PPair _ a' b') =
  annihilate a a' && annihilate b b'
annihilate (PRecord _ as) (PRecord _ bs) =
  let diffKeys = S.difference (M.keysSet as) (M.keysSet bs)
   in S.null diffKeys
        && do
          let allPairs = zip (M.elems as) (M.elems bs)
          foldr
            (\(a, b) keep -> keep && annihilate a b)
            True
            allPairs
annihilate (PConstructor _ tyConA argsA) (PConstructor _ tyConB argsB) =
  (tyConA == tyConB)
    && foldr
      (\(a, b) keep -> keep && annihilate a b)
      True
      (zip argsA argsB)
annihilate _ _as = False

redundantCases ::
  (MonadError TypeError m, Eq var) =>
  Environment ->
  [Pattern var Annotation] ->
  m [Pattern var Annotation]
redundantCases env patterns = do
  generated <-
    mconcat
      <$> traverse (generate env) patterns
  let annihiliatePattern pat remaining =
        filter
          ( not
              . annihilate
                (removeAnn pat)
              . removeAnn
          )
          remaining
  -- add index, the first pattern is never redundant
  let patternsWithIndex = zip patterns ([0 ..] :: [Int])
  pure $
    snd $
      foldl'
        ( \(remaining, redundant) (pat, i) ->
            let rest = annihiliatePattern pat remaining
             in if length rest == length remaining && i > 0
                  then (rest, redundant <> [pat])
                  else (rest, redundant)
        )
        (generated, mempty)
        patternsWithIndex
