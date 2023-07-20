module Smol.Core.Interpreter.Infix (interpretInfix) where

import Control.Monad (void, (<=<))
import Control.Monad.Except
import Smol.Core.Interpreter.Types
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Types.Expr
import Smol.Core.Types.Op
import Smol.Core.Types.Prim

-- | this assumes that
interpretInfix ::
  (Monoid ann) =>
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
      let withBool = pure . EPrim mempty . PBool
      if void plainA == void plainB
        then withBool True
        else withBool False
    OpAdd -> do
      let withInt = pure . EPrim mempty . PInt
          getInt exp' = case exp' of
            (EPrim _ (PInt i)) -> Right i
            _ -> Left $ AdditionWithNonNumber a
          withNat = pure . EPrim mempty . PNat
          getNat exp' = case exp' of
            (EPrim _ (PNat i)) -> Right i
            _ -> Left $ AdditionWithNonNumber a
      case (,) <$> getInt plainA <*> getInt plainB of
        Right (a', b') -> withInt (a' + b')
        Left _ -> case (,) <$> getNat plainA <*> getNat plainB of
          Right (a', b') -> withNat (a' + b')
          Left e -> throwError e
