module Smol.Core.Interpreter.Interpret (interpret, addEmptyStackFrames) where

import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Smol.Core.Interpreter.App
import Smol.Core.Interpreter.If
import Smol.Core.Interpreter.Infix
import Smol.Core.Interpreter.Let
import Smol.Core.Interpreter.Monad
import Smol.Core.Interpreter.PatternMatch
import Smol.Core.Interpreter.RecordAccess
import Smol.Core.Interpreter.Types
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Types

initialStack :: StackFrame ann
initialStack = StackFrame mempty

addEmptyStackFrames ::
  Expr dep ann ->
  Expr dep (ExprData ann)
addEmptyStackFrames expr =
  fmap
    ( \ann ->
        ExprData
          { edIsRecursive = False,
            edStackFrame = mempty,
            edAnnotation = ann
          }
    )
    expr

interpret ::
  (Eq ann, Show ann) =>
  Map (ResolvedDep Identifier) (InterpretExpr ann) ->
  InterpretExpr ann ->
  Either (InterpreterError ann) (InterpretExpr ann)
interpret deps expr =
  runReaderT (interpretExpr expr) (InterpretReaderEnv initialStack deps)

-- somewhat pointless separate function to make debug logging each value out
-- easier
interpretExpr ::
  (Eq ann, Show ann) =>
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretExpr =
  interpretExpr'

interpretExpr' ::
  (Eq ann, Show ann) =>
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretExpr' (EPrim ann val) = pure (EPrim ann val)
interpretExpr' (EAnn _ _ expr) = interpretExpr' expr
interpretExpr' (ELet exprData ident expr body) =
  interpretLet interpretExpr (ident, exprData) expr body
interpretExpr' (EVar _ var) =
  lookupVar var >>= interpretExpr
interpretExpr' (ELambda (ExprData current isRec ann) ident body) = do
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
    (ELambda newExprData ident body)
interpretExpr' (ETuple ann a as) =
  ETuple ann <$> interpretExpr a <*> traverse interpretExpr as
interpretExpr' (EInfix _ op a b) =
  interpretInfix interpretExpr op a b
interpretExpr' (EIf ann predExpr thenExpr elseExpr) =
  interpretIf interpretExpr ann predExpr thenExpr elseExpr
interpretExpr' (EApp ann fn a) =
  interpretApp interpretExpr ann fn a
interpretExpr' (ERecordAccess ann expr name) =
  interpretRecordAccess interpretExpr ann expr name
interpretExpr' (EPatternMatch _ matchExpr patterns) = do
  interpretPatternMatch interpretExpr matchExpr (NE.toList patterns)
interpretExpr' (ERecord ann as) =
  ERecord ann <$> traverse interpretExpr as
interpretExpr' (EArray ann as) =
  EArray ann <$> traverse interpretExpr as
interpretExpr' (EConstructor as const') =
  pure (EConstructor as const')
