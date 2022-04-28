module Language.Mimsa.Interpreter.App (interpretApp) where

import Language.Mimsa.Interpreter.Monad
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Interpreter.Stack

varFromIdent :: Identifier var ann -> var
varFromIdent (Identifier _ var) = var

interpretApp ::
  (Ord var, Eq ann) =>
  InterpretFn var ann ->
  ExprData var ann ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretApp interpretFn ann myFn value =
  case myFn of
    (MyLambda (ExprData closure _ _) ident body) -> do
      -- interpret arg first
      intValue <- interpretFn value
      -- add arg to context
      let newStackFrame = addVarToFrame (varFromIdent ident) intValue closure
      -- run body with closure + new arg
      withNewStackFrame newStackFrame (interpretFn body)
    (MyConstructor ann' const') ->
      MyApp ann (MyConstructor ann' const')
        <$> interpretFn value
    fn -> do
      -- try and resolve it into something we recognise
      intFn <- interpretFn fn
      if intFn == fn -- if it hasn't changed, we don't want to end up looping so give up and error
        then do
          intValue <- interpretFn value
          -- at least change the value
          pure (MyApp ann intFn intValue)
        else interpretFn (MyApp ann intFn value)
