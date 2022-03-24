module Language.Mimsa.Interpreter2.App (interpretApp) where

import Control.Monad.Reader
import Language.Mimsa.Interpreter2.Monad
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Interpreter.Stack

varFromIdent :: Identifier var ann -> var
varFromIdent (Identifier _ var) = var
varFromIdent (AnnotatedIdentifier _ var) = var

interpretApp ::
  (Ord var) =>
  InterpretFn var ann ->
  StackFrame var ann ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretApp interpretFn ann (MyVar _ var) a = do
  intF <- lookupVar var
  interpretFn (MyApp ann intF a)
interpretApp interpretFn _ (MyLambda closure ident body) a = do
  -- interpret arg first
  intA <- interpretFn a
  -- add arg to context
  let newStackFrame = addVarToFrame (varFromIdent ident) intA closure
  -- run body with closure + new arg
  local (addStackFrame newStackFrame) (interpretFn body)
interpretApp interpretFn ann (MyConstructor ann' const') value =
  MyApp ann (MyConstructor ann' const')
    <$> interpretFn value
interpretApp interpretFn ann other a = do
  -- try and resolve it into something we recognise
  unfoldedF <- interpretFn other
  if unfoldedF == other -- if it hasn't changed, we don't want to end up looping so give up and error
    then pure (MyApp ann unfoldedF a)
    else interpretFn (MyApp ann unfoldedF a)
