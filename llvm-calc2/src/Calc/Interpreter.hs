{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc.Interpreter (interpret, InterpreterError (..)) where

import Calc.Types
import Control.Monad.Except

data InterpreterError ann
  = NonBooleanPredicate ann (Expr ann)
  deriving stock (Eq, Ord, Show)

interpretInfix ::
  (MonadError (InterpreterError ann) m) =>
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  m (Expr ann)
interpretInfix ann OpAdd (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a + b)
interpretInfix ann OpSubtract (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a - b)
interpretInfix ann OpMultiply (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a * b)
interpretInfix ann OpEquals (EPrim _ a) (EPrim _ b) =
  pure $ EPrim ann (PBool $ a == b)
interpretInfix ann op a b = do
  iA <- interpret a
  iB <- interpret b
  interpretInfix ann op iA iB

-- | just keep reducing the thing until the smallest thing
interpret ::
  ( MonadError (InterpreterError ann) m
  ) =>
  Expr ann ->
  m (Expr ann)
interpret (EPrim ann p) = pure (EPrim ann p)
interpret (EInfix ann op a b) =
  interpretInfix ann op a b
interpret (EIf ann predExpr thenExpr elseExpr) = do
  predA <- interpret predExpr
  case predA of
    (EPrim _ (PBool True)) -> interpret thenExpr
    (EPrim _ (PBool False)) -> interpret elseExpr
    other -> throwError (NonBooleanPredicate ann other)
