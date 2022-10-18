{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.Solve (solve, runSolveM, SolveM) where
import Language.Mimsa.Types.Typechecker.Substitutions

import Control.Monad.Except
import Control.Monad.State
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker

type SolveM = ExceptT TypeError (State TypecheckState)

runSolveM ::
  TypecheckState ->
  SolveM a ->
  Either TypeError (TypecheckState, a)
runSolveM tcState value =
  case either' of
    (Right a, newTcState) -> Right (newTcState, a)
    (Left e, _) -> Left e
  where
    either' =
      runState
        (runExceptT value)
        tcState

solve ::
  ( MonadState TypecheckState m,
    MonadError TypeError m
  ) =>
  [Constraint] ->
  m Substitutions
solve = go mempty
  where
    go s [] = pure s
    go s1 (constraint : rest) =
      case constraint of
        ShouldEqual a b -> do
          s2 <- unify a b
          go (s2 <> s1) (applyToConstraint (s1 <> s2) <$> rest)

applyToConstraint :: Substitutions -> Constraint -> Constraint
applyToConstraint subs (ShouldEqual a b) =
  ShouldEqual (applySubst subs a) (applySubst subs b)
