module Language.Mimsa.Interpreter.Interpret (interpret) where

import Control.Monad.Reader
import Data.Bifunctor
import Data.Functor
import Data.Map (Map)
import Language.Mimsa.Interpreter.App
import Language.Mimsa.Interpreter.If
import Language.Mimsa.Interpreter.Infix
import Language.Mimsa.Interpreter.Let
import Language.Mimsa.Interpreter.Monad
import Language.Mimsa.Interpreter.PatternMatch
import Language.Mimsa.Interpreter.RecordAccess
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash

initialStack :: (Ord var) => StackFrame var ann
initialStack = StackFrame mempty mempty

interpret ::
  (Eq ann, Ord var, Show var, Printer var, Monoid ann) =>
  Map ExprHash (Expr (var, Maybe ExprHash) ann) ->
  Expr (var, Maybe ExprHash) ann ->
  Either (InterpreterError var ann) (Expr var ann)
interpret deps expr =
  let addEmptyStackFrame exp' = exp' $> mempty
      expr' = addEmptyStackFrame expr
      initialDeps = addEmptyStackFrame <$> deps
      removeExprData = bimap fst edAnnotation
   in removeExprData <$> runReaderT (interpretExpr expr') (InterpretReaderEnv initialStack initialDeps)

-- somewhat pointless separate function to make debug logging each value out
-- easier
interpretExpr ::
  (Eq ann, Ord var, Show var, Printer var, Monoid ann) =>
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretExpr =
  interpretExpr'

interpretExpr' ::
  (Eq ann, Ord var, Show var, Printer var, Monoid ann) =>
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretExpr' (MyLiteral _ val) = pure (MyLiteral mempty val)
interpretExpr' (MyLet _ ident expr body) =
  interpretLet interpretExpr ident expr body
interpretExpr' (MyVar _ var) =
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
interpretExpr' (MyPair ann a b) =
  MyPair ann <$> interpretExpr a <*> interpretExpr b
interpretExpr' (MyInfix _ op a b) =
  interpretInfix interpretExpr op a b
interpretExpr' (MyIf ann predExpr thenExpr elseExpr) =
  interpretIf interpretExpr ann predExpr thenExpr elseExpr
interpretExpr' (MyApp ann fn a) =
  interpretApp interpretExpr ann fn a
interpretExpr' (MyRecordAccess ann expr name) =
  interpretRecordAccess interpretExpr ann expr name
interpretExpr' (MyDefineInfix _ op fn expr) = do
  intFn <- interpretExpr fn
  addOperator op intFn (interpretExpr expr)
interpretExpr' (MyData _ _ expr) = interpretExpr expr
interpretExpr' (MyPatternMatch _ matchExpr patterns) = do
  interpretPatternMatch interpretExpr matchExpr patterns
interpretExpr' (MyLetPattern _ pat patExpr body) =
  interpretLetPattern interpretExpr pat patExpr body
interpretExpr' (MyRecord ann as) =
  MyRecord ann <$> traverse interpretExpr as
interpretExpr' (MyArray ann as) =
  MyArray ann <$> traverse interpretExpr as
interpretExpr' (MyConstructor as const') =
  pure (MyConstructor as const')
interpretExpr' (MyTypedHole ann name) =
  pure (MyTypedHole ann name)
