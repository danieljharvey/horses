module Smol.Core.Interpreter.Infix (interpretInfix) where

import Control.Monad (void, (<=<))
import Control.Monad.Except
import Smol.Core.Interpreter.Types
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Typecheck.Shared
import Smol.Core.Types.Expr
import Smol.Core.Types.Op
import Smol.Core.Types.Prim

-- | this assumes that
interpretInfix ::
  InterpretFn ann ->
  Op ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretInfix interpretFn operator a b = do
  plainA <- interpretFn <=< interpretFn $ a
  plainB <- interpretFn <=< interpretFn $ b
  case operator of
    OpEquals -> do
      let withBool = pure . EPrim (getExprAnnotation a) . PBool
      if void plainA == void plainB
        then withBool True
        else withBool False
    OpAdd -> do
      let withInt = pure . EPrim (getExprAnnotation a) . PInt
          getInt exp' = case exp' of
            (EPrim _ (PInt i)) -> Right i
            _ -> Left $ AdditionWithNonNumber a
      case (,) <$> getInt plainA <*> getInt plainB of
        Right (a', b') -> withInt (a' + b')
        Left e -> throwError e
