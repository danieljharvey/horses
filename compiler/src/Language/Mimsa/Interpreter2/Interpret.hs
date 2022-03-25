module Language.Mimsa.Interpreter2.Interpret (interpret) where

import Control.Monad.Reader
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
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
import Language.Mimsa.Types.Store.ExprHash

varFromIdent :: Identifier var ann -> var
varFromIdent (Identifier _ var) = var
varFromIdent (AnnotatedIdentifier _ var) = var

initialStack :: (Ord var) => Stack var ann
initialStack = Stack (NE.singleton (StackFrame mempty mempty))

interpret ::
  (Eq ann, Ord var, Show var) =>
  Map ExprHash (Expr (var, Maybe ExprHash) ann) ->
  Expr (var, Maybe ExprHash) ann ->
  Either (InterpreterError2 var ann) (InterpretExpr var ann)
interpret deps expr =
  let addEmptyStackFrame exp' = exp' $> mempty
      expr' = addEmptyStackFrame expr
      initialDeps = addEmptyStackFrame <$> deps
   in runReaderT (interpretExpr expr') (InterpretReaderEnv initialStack initialDeps)

interpretExpr ::
  (Eq ann, Ord var, Show var) =>
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretExpr (MyLiteral _ val) = pure (MyLiteral mempty val)
interpretExpr (MyLet _ ident expr body) = do
  -- calc expr, including itself to sort recursion
  intExpr <-
    addToStackFrame
      (varFromIdent ident)
      expr
      (interpretExpr expr)
  -- calc rest, with new binding added to the current stack frame
  addToStackFrame
    (varFromIdent ident)
    intExpr
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
  addOperator
    op
    fn
    (interpretExpr expr)
interpretExpr (MyData _ _ expr) = interpretExpr expr
interpretExpr (MyPatternMatch _ matchExpr patterns) = do
  intMatchExpr <- interpretExpr matchExpr
  interpretPatternMatch interpretExpr intMatchExpr patterns
interpretExpr (MyLetPattern _ pat patExpr body) =
  interpretLetPattern interpretExpr pat patExpr body
interpretExpr (MyRecord ann as) = MyRecord ann <$> traverse interpretExpr as
interpretExpr (MyArray ann as) = MyArray ann <$> traverse interpretExpr as
interpretExpr (MyConstructor as const') = pure (MyConstructor as const')
interpretExpr (MyTypedHole ann name) = pure (MyTypedHole ann name)
