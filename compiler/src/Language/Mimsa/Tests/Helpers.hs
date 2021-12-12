{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Tests.Helpers (unifies) where

import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker

-- do these two types work together?
unifies ::
  MonoType ->
  MonoType ->
  Either TypeError ()
unifies mtA mtB = do
  _ <-
    runSolveM
      mempty
      defaultTcState
      (unify mtA mtB)
  pure ()
