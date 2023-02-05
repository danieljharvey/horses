{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Interpreter.Interpret (interpret) where

import Calc.Types

interpretInfix ::
  (Monad m) =>
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  m (Expr ann)
interpretInfix ann Add (EPrim _ a ) (EPrim _ b ) =
  pure $ EPrim ann (a + b)
interpretInfix _ _ _ _ = error "haven't implemented other infixes"

-- | just keep reducing the thing until the smallest thing
interpret ::
  ( Monad m,
    Monoid ann,
    Show ann
  ) =>
  Expr ann ->
  m (Expr ann)
interpret (EPrim ann p) = pure (EPrim ann p)
interpret (EInfix ann op a b) = do
  rA <- interpret a
  rB <- interpret b
  interpretInfix ann op rA rB

