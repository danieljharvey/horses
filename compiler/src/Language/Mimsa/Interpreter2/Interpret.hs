module Language.Mimsa.Interpreter2.Interpret (interpret) where

import Control.Monad.Reader
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Language.Mimsa.Interpreter2.Infix
import Language.Mimsa.Interpreter2.Monad
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Interpreter.Stack

varFromIdent :: Identifier var ann -> var
varFromIdent (Identifier _ var) = var
varFromIdent (AnnotatedIdentifier _ var) = var

initialStack :: (Ord var) => Stack var ann
initialStack = Stack (NE.singleton (StackFrame mempty mempty))

interpret ::
  (Eq ann, Ord var, Show var) =>
  Expr var ann ->
  Either (InterpreterError2 var ann) (InterpretExpr var ann)
interpret expr =
  let expr' = expr $> mempty
   in runReaderT (interpretAction expr') initialStack

interpretAction ::
  (Eq ann, Ord var, Show var) =>
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretAction (MyLiteral _ val) = pure (MyLiteral mempty val)
interpretAction (MyLet _ ident expr body) = do
  -- calc expr body
  interpretedExpr <- interpretAction expr
  -- calc rest, with new binding added to the current stack frame
  local
    (addToStackFrame (varFromIdent ident) interpretedExpr)
    (interpretAction body)
interpretAction (MyVar _ var) = lookupVar var
interpretAction (MyLambda _ ident body) = do
  -- capture current environment
  closure <- getCurrentStackFrame
  pure (MyLambda closure ident body)
interpretAction (MyPair ann a b) =
  MyPair ann <$> interpretAction a <*> interpretAction b
interpretAction (MyInfix _ op a b) = interpretInfix interpretAction op a b
interpretAction (MyApp ann fn a) = do
  case fn of
    (MyVar _ var) -> do
      intF <- lookupVar var
      interpretAction (MyApp ann intF a)
    (MyLambda closure ident body) -> do
      -- interpret arg first
      intA <- interpretAction a
      -- add arg to context
      let newStackFrame = addVarToFrame (varFromIdent ident) intA closure
      -- run body with closure + new arg
      local (addStackFrame newStackFrame) (interpretAction body)
    other -> do
      -- try and resolve it into something we recognise
      unfoldedF <- interpretAction other
      if unfoldedF == other -- if it hasn't changed, we don't want to end up looping so give up and error
        then error "Unfolding failed, what is going on here"
        else interpretAction (MyApp ann unfoldedF a)
interpretAction a = error (show a <> " not implemented")
