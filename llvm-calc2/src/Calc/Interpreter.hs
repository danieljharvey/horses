module Calc.Interpreter (interpret) where

import Calc.Types

interpretInfix ::
  (Monad m) =>
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
  ( Monad m
  ) =>
  Expr ann ->
  m (Expr ann)
interpret (EPrim ann p) = pure (EPrim ann p)
interpret (EInfix ann op a b) =
  interpretInfix ann op a b
