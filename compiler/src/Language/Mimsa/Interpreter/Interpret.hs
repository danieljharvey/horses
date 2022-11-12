module Language.Mimsa.Interpreter.Interpret (interpret, addEmptyStackFrames) where

import Control.Monad.Reader
import Data.Functor
import Data.Map.Strict (Map)
import Language.Mimsa.Core
import Language.Mimsa.Interpreter.App
import Language.Mimsa.Interpreter.If
import Language.Mimsa.Interpreter.Infix
import Language.Mimsa.Interpreter.PatternMatch
import Language.Mimsa.Interpreter.RecordAccess
import Language.Mimsa.Interpreter.Types
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker.Unique
import Language.Mimsa.Types.Identifiers

addEmptyStackFrames ::
  (Monoid ann) =>
  Regular.Expr (var, Unique) ann ->
  Regular.Expr (var, Unique) (ExprData ann)
addEmptyStackFrames expr =
  expr $> mempty

interpret ::
  (Eq ann, Monoid ann, Show ann) =>
  Map ExprHash (InterpretExpr ann) ->
  Map InfixOp ExprHash ->
  InterpretExpr ann ->
  Either (InterpreterError Name ann) (InterpretExpr ann)
interpret deps infixes expr =
  runReaderT (interpretExpr expr) (InterpretReaderEnv deps infixes)

-- somewhat pointless separate function to make debug logging each value out
-- easier
interpretExpr ::
  (Eq ann, Monoid ann, Show ann) =>
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretExpr =
  interpretExpr'

interpretExpr' ::
  (Eq ann, Monoid ann, Show ann) =>
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretExpr' (HOAS.MyLiteral _ val) = pure (HOAS.MyLiteral mempty val)
interpretExpr' (HOAS.MyAnnotation _ _ expr) = interpretExpr' expr
interpretExpr' (HOAS.MyVar ann modName var) =
  -- lookupVar var >>= interpretExpr
  pure (HOAS.MyVar ann modName var) -- TODO: is this right?
interpretExpr' (HOAS.MyLambda exprData ident body) = do
  -- return it
  pure
    (HOAS.MyLambda exprData ident body)
interpretExpr' (HOAS.MyTuple ann a as) =
  HOAS.MyTuple ann <$> interpretExpr a <*> traverse interpretExpr as
interpretExpr' (HOAS.MyInfix _ op a b) =
  interpretInfix interpretExpr op a b
interpretExpr' (HOAS.MyIf ann predExpr thenExpr elseExpr) =
  interpretIf interpretExpr ann predExpr thenExpr elseExpr
interpretExpr' (HOAS.MyApp ann fn a) =
  interpretApp interpretExpr ann fn a
interpretExpr' (HOAS.MyRecordAccess ann expr name) =
  interpretRecordAccess interpretExpr ann expr name
interpretExpr' (HOAS.MyTupleAccess ann expr index) =
  interpretTupleAccess interpretExpr ann expr index
interpretExpr' (HOAS.MyPatternMatch _ matchExpr patterns) = do
  interpretPatternMatch interpretExpr matchExpr patterns
interpretExpr' (HOAS.MyLetPattern _ pat patExpr body) =
  interpretLetPattern interpretExpr pat patExpr body
interpretExpr' (HOAS.MyRecord ann as) =
  HOAS.MyRecord ann <$> traverse interpretExpr as
interpretExpr' (HOAS.MyArray ann as) =
  HOAS.MyArray ann <$> traverse interpretExpr as
interpretExpr' (HOAS.MyConstructor as modName const') =
  pure (HOAS.MyConstructor as modName const')
interpretExpr' (HOAS.MyTypedHole ann name) =
  pure (HOAS.MyTypedHole ann name)
