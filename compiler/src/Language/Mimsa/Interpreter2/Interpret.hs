module Language.Mimsa.Interpreter2.Interpret (interpret) where

import Control.Monad.Reader
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Language.Mimsa.ExprUtils
import Language.Mimsa.Interpreter2.App
import Language.Mimsa.Interpreter2.If
import Language.Mimsa.Interpreter2.Infix
import Language.Mimsa.Interpreter2.Monad
import Language.Mimsa.Interpreter2.PatternMatch
import Language.Mimsa.Interpreter2.RecordAccess
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Interpreter.Stack

varFromIdent :: Identifier var ann -> var
varFromIdent (Identifier _ var) = var
varFromIdent (AnnotatedIdentifier _ var) = var

-- we are currently including all the deps in with the regular stuff
-- this means we are going to make loads of copies of it
-- which doesn't feel efficient
-- perhaps instead the state should contain the stack for scoped stuff
-- and then a flat set of Map ExprHash Expr which is static throughout
initialStack :: Map var (InterpretExpr var ann) -> Stack var ann
initialStack deps = Stack (NE.singleton (StackFrame deps mempty))

interpret ::
  (Eq ann, Ord var, Show var) =>
  Map var (Expr var ann) ->
  Expr var ann ->
  Either (InterpreterError2 var ann) (InterpretExpr var ann)
interpret deps expr =
  let addEmptyStackFrame exp' = exp' $> mempty
      expr' = addEmptyStackFrame expr
      initialDeps = addEmptyStackFrame <$> deps
   in runReaderT (interpretExpr expr') (initialStack initialDeps)

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
interpretExpr (MyInfix _ op a b) =
  interpretInfix interpretExpr op a b
interpretExpr (MyIf ann predExpr thenExpr elseExpr) =
  interpretIf interpretExpr ann predExpr thenExpr elseExpr
interpretExpr (MyApp ann fn a) =
  interpretApp interpretExpr ann fn a
interpretExpr (MyRecordAccess ann expr name) =
  interpretRecordAccess interpretExpr ann expr name
interpretExpr (MyDefineInfix _ op fn expr) =
  local
    (addOperator op fn)
    (interpretExpr expr)
interpretExpr (MyData _ _ expr) = interpretExpr expr
interpretExpr (MyPatternMatch _ matchExpr patterns) = do
  intMatchExpr <- interpretExpr matchExpr
  interpretPatternMatch interpretExpr intMatchExpr patterns
interpretExpr (MyLetPattern _ pat patExpr body) =
  interpretLetPattern interpretExpr pat patExpr body
interpretExpr other =
  bindExpr interpretExpr other -- handle all other cases by just interpreting each subexpression
