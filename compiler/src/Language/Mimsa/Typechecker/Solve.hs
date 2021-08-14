{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Solve (solve, runSolveM, SolveM) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Language.Mimsa.Logging
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type SolveM = ExceptT TypeError (ReaderT Swaps (State TypecheckState))

runSolveM ::
  Swaps ->
  TypecheckState ->
  SolveM a ->
  Either TypeError (TypecheckState, a)
runSolveM swaps tcState value =
  case either' of
    (Right a, newTcState) -> Right (newTcState, a)
    (Left e, _) -> Left e
  where
    either' =
      runState
        (runReaderT (runExceptT value) swaps)
        tcState

solve ::
  ( MonadState TypecheckState m,
    MonadReader Swaps m,
    MonadError TypeError m
  ) =>
  [Constraint] ->
  m Substitutions
solve = go mempty
  where
    go s [] = pure s
    go s1 (lc : rest) =
      case debugPretty "constraint" lc of
        ShouldEqual a b -> do
          s2 <- unify a b
          go (s2 <> s1) (applyToConstraint s2 <$> rest)

applyToConstraint :: Substitutions -> Constraint -> Constraint
applyToConstraint subs (ShouldEqual a b) =
  ShouldEqual (applySubst subs a) (applySubst subs b)
