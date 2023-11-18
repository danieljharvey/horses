module Smol.Interpreter.App (interpretApp) where

import Smol.Core.Types.Expr
import Smol.Interpreter.Monad
import Smol.Interpreter.Types
import Smol.Interpreter.Types.Stack

interpretApp ::
  (Eq ann) =>
  InterpretFn ann ->
  ExprData ann ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretApp interpretFn ann myFn value =
  case myFn of
    (ELambda (ExprData closure _ _) ident body) -> do
      -- interpret arg first
      intValue <- interpretFn value
      -- add arg to context
      let newStackFrame = addVarToFrame ident intValue closure
      -- run body with closure + new arg
      withNewStackFrame newStackFrame (interpretFn body)
    (EConstructor ann' const') ->
      EApp ann (EConstructor ann' const')
        <$> interpretFn value
    fn -> do
      -- try and resolve it into something we recognise
      intFn <- interpretFn fn
      if intFn == fn -- if it hasn't changed, we don't want to end up looping so give up and error
        then do
          intValue <- interpretFn value
          -- at least change the value
          pure (EApp ann intFn intValue)
        else interpretFn (EApp ann intFn value)
