module Language.Mimsa.Interpreter2.Interpret (interpret) where

import Control.Monad.Reader
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Language.Mimsa.Interpreter2.If
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
   in runReaderT (interpretExpr expr') initialStack

interpretExpr ::
  (Eq ann, Ord var, Show var) =>
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretExpr (MyLiteral _ val) = pure (MyLiteral mempty val)
interpretExpr (MyLet _ ident expr body) = do
  -- calc expr body
  interpretedExpr <- interpretExpr expr
  -- calc rest, with new binding added to the current stack frame
  local
    (addToStackFrame (varFromIdent ident) interpretedExpr)
    (interpretExpr body)
interpretExpr (MyVar _ var) = lookupVar var
interpretExpr (MyLambda _ ident body) = do
  -- capture current environment
  closure <- getCurrentStackFrame
  pure (MyLambda closure ident body)
interpretExpr (MyPair ann a b) =
  MyPair ann <$> interpretExpr a <*> interpretExpr b
interpretExpr (MyInfix _ op a b) = interpretInfix interpretExpr op a b
interpretExpr (MyRecord ann exprs) =
  MyRecord ann <$> traverse interpretExpr exprs
interpretExpr (MyIf ann predExpr thenExpr elseExpr) =
  interpretIf interpretExpr ann predExpr thenExpr elseExpr
interpretExpr (MyApp ann fn a) = do
  case fn of
    (MyVar _ var) -> do
      intF <- lookupVar var
      interpretExpr (MyApp ann intF a)
    (MyLambda closure ident body) -> do
      -- interpret arg first
      intA <- interpretExpr a
      -- add arg to context
      let newStackFrame = addVarToFrame (varFromIdent ident) intA closure
      -- run body with closure + new arg
      local (addStackFrame newStackFrame) (interpretExpr body)
    other -> do
      -- try and resolve it into something we recognise
      unfoldedF <- interpretExpr other
      if unfoldedF == other -- if it hasn't changed, we don't want to end up looping so give up and error
        then error "Unfolding failed, what is going on here"
        else interpretExpr (MyApp ann unfoldedF a)
interpretExpr a = error (show a <> " not implemented")
