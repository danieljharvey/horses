module Smol.Core.Interpreter.Interpret (interpret, addEmptyStackFrames) where

import Control.Monad.Reader
import Data.Functor
import Data.Map.Strict (Map)
import Smol.Core.Interpreter.App
import Smol.Core.Interpreter.If
import Smol.Core.Interpreter.Infix
import Smol.Core.Interpreter.Let
import Smol.Core.Interpreter.Monad
import Smol.Core.Interpreter.PatternMatch
import Smol.Core.Interpreter.RecordAccess
import Smol.Core.Interpreter.Types
import Smol.Core.Types
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Interpreter.Types.Stack

initialStack :: (Ord var) => StackFrame var ann
initialStack = StackFrame mempty mempty

addEmptyStackFrames ::
  (Ord var, Monoid ann) =>
  Expr (var, Unique) ann ->
  Expr (var, Unique) (ExprData var ann)
addEmptyStackFrames expr =
  expr $> mempty

interpret ::
  (Eq ann, Ord var, Show var, Printer var, Monoid ann, Show ann) =>
  Map ExprHash (InterpretExpr var ann) ->
  Map InfixOp ExprHash ->
  InterpretExpr var ann ->
  Either (InterpreterError var ann) (InterpretExpr var ann)
interpret deps infixes expr =
  runReaderT (interpretExpr expr) (InterpretReaderEnv initialStack deps infixes)

-- somewhat pointless separate function to make debug logging each value out
-- easier
interpretExpr ::
  (Eq ann, Ord var, Show var, Printer var, Monoid ann, Show ann) =>
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretExpr =
  interpretExpr'

interpretExpr' ::
  (Eq ann, Ord var, Show var, Printer var, Monoid ann, Show ann) =>
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretExpr' (MyLiteral _ val) = pure (MyLiteral mempty val)
interpretExpr' (MyAnnotation _ _ expr) = interpretExpr' expr
interpretExpr' (MyLet _ ident expr body) =
  interpretLet interpretExpr ident expr body
interpretExpr' (MyVar _ _ var) =
  lookupVar var >>= interpretExpr
interpretExpr' (MyLambda (ExprData current isRec ann) ident body) = do
  -- capture current environment
  stackFrame <-
    getCurrentStackFrame
  -- add it to already captured vars
  let newExprData =
        ExprData
          (current <> stackFrame)
          isRec
          ann
  -- return it
  pure
    (MyLambda newExprData ident body)
interpretExpr' (MyTuple ann a as) =
  MyTuple ann <$> interpretExpr a <*> traverse interpretExpr as
interpretExpr' (MyInfix _ op a b) =
  interpretInfix interpretExpr op a b
interpretExpr' (MyIf ann predExpr thenExpr elseExpr) =
  interpretIf interpretExpr ann predExpr thenExpr elseExpr
interpretExpr' (MyApp ann fn a) =
  interpretApp interpretExpr ann fn a
interpretExpr' (MyRecordAccess ann expr name) =
  interpretRecordAccess interpretExpr ann expr name
interpretExpr' (MyTupleAccess ann expr index) =
  interpretTupleAccess interpretExpr ann expr index
interpretExpr' (MyPatternMatch _ matchExpr patterns) = do
  interpretPatternMatch interpretExpr matchExpr patterns
interpretExpr' (MyLetPattern _ pat patExpr body) =
  interpretLetPattern interpretExpr pat patExpr body
interpretExpr' (MyRecord ann as) =
  MyRecord ann <$> traverse interpretExpr as
interpretExpr' (MyArray ann as) =
  MyArray ann <$> traverse interpretExpr as
interpretExpr' (MyConstructor as modName const') =
  pure (MyConstructor as modName const')
interpretExpr' (MyTypedHole ann name) =
  pure (MyTypedHole ann name)
