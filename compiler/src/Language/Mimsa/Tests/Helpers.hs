{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Tests.Helpers (toMonadError, unifies, exprEqualsTrue, testIsSuccess) where

import Control.Monad.Except
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker

toMonadError :: (MonadError e m) => Either e a -> m a
toMonadError = \case
  Right a -> pure a
  Left e -> throwError e

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

-- | take response of test and == it with True to make it is easy to check for
-- truthiness
exprEqualsTrue ::
  (Monoid ann) =>
  Expr var ann ->
  Expr var ann
exprEqualsTrue =
  MyInfix
    mempty
    Equals
    (MyLiteral mempty (MyBool True))

-- | did the test succeed?
testIsSuccess :: Expr var ann -> Bool
testIsSuccess (MyLiteral _ (MyBool True)) = True
testIsSuccess _ = False
