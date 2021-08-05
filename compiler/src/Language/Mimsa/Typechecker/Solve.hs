{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Solve (solve, runSolveM, SolveM) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
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

applySubstScheme :: Substitutions -> Scheme -> Scheme
applySubstScheme (Substitutions subst) (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars (applySubst newSubst t)
  where
    newSubst = Substitutions (foldr M.delete subst vars)

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
      case lc of
        ShouldEqual a b -> do
          s2 <- unify a b
          go (s2 <> s1) (applyToConstraint s2 <$> rest)
        InstanceOf a scheme -> do
          (_, tyA) <- instantiate scheme
          go s1 $ [ShouldEqual a tyA] <> rest

applyToConstraint :: Substitutions -> Constraint -> Constraint
applyToConstraint subs (ShouldEqual a b) =
  ShouldEqual (applySubst subs a) (applySubst subs b)
applyToConstraint subs (InstanceOf a b) =
  InstanceOf (applySubst subs a) (applySubstScheme subs b)

instantiate ::
  (MonadState TypecheckState m) =>
  Scheme ->
  m (Substitutions, MonoType)
instantiate (Scheme vars ty) = do
  newVars <- traverse (const $ getUnknown mempty) vars
  let pairs = zip vars newVars
  let subst = debugPretty "instantiate" (Substitutions $ M.fromList pairs)
  pure (subst, debugPretty "instantiated" (applySubst subst ty))
