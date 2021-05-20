{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Typechecker.Exhaustiveness where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Data.List (nub)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker.Environment

type PatternM = ExceptT TypeError (ReaderT Environment (State PatternState))

newtype PatternState = PatternState
  { psSeen :: Set TyCon
  }
  deriving newtype (Semigroup, Monoid)

addSeen :: TyCon -> PatternM ()
addSeen tyCon = modify (\st -> st {psSeen = psSeen st <> S.singleton tyCon})

runPatternM ::
  Environment ->
  PatternM a ->
  Either TypeError a
runPatternM env value =
  fst either'
  where
    either' =
      runState
        (runReaderT (runExceptT value) env)
        mempty

-- | given a list of patterns, return a list of missing patterns
isExhaustive ::
  (Eq var) =>
  [Pattern var Annotation] ->
  PatternM [Pattern var Annotation]
isExhaustive patterns = do
  generated <-
    mconcat
      <$> traverse generateRequired patterns
  pure $ filterMissing patterns generated

-- | Given a pattern, generate others required for it
generateRequired ::
  Pattern var Annotation ->
  PatternM [Pattern var Annotation]
generateRequired (PLit _ (MyBool True)) = pure [PLit mempty (MyBool False)]
generateRequired (PLit _ (MyBool False)) = pure [PLit mempty (MyBool True)]
generateRequired (PLit _ (MyInt _)) = pure [PWildcard mempty]
generateRequired (PLit _ (MyString _)) = pure [PWildcard mempty]
generateRequired (PPair _ l r) = do
  ls <- generateRequired l
  rs <- generateRequired r
  let allPairs = PPair mempty <$> ls <*> rs
  pure allPairs
generateRequired (PRecord _ items) = do
  items' <- traverse generateRequired items
  pure (PRecord mempty <$> sequence items')
generateRequired (PConstructor ann tyCon args) = do
  env <- ask
  dt <- lookupConstructor env ann tyCon
  newFromArgs <- traverse generateRequired args
  newDataTypes <- requiredFromDataType dt
  let newCons = PConstructor mempty tyCon <$> sequence newFromArgs
  pure (newCons <> newDataTypes)
generateRequired _ = pure mempty

requiredFromDataType ::
  DataType Annotation ->
  PatternM [Pattern var Annotation]
requiredFromDataType (DataType _ _ cons) = do
  seen <- gets psSeen
  let new (n, as) =
        ( [ PConstructor
              mempty
              n
              (PWildcard mempty <$ as)
            | n `notElem` seen
          ]
        )
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
          foldr (\(a, b) keep -> keep && annihilate a b) True allPairs
annihilate _ _as = False
