module Smol.Interpreter.Infix (interpretInfix) where

import Control.Monad (void, (<=<))
import Control.Monad.Except
import Smol.Interpreter.Types
import Smol.Interpreter.Types.InterpreterError
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
    OpAdd -> addInts plainA plainB `catchError` \_ -> concatStrings plainA plainB

addInts :: InterpretExpr ann -> InterpretExpr ann -> InterpreterM ann (InterpretExpr ann)
addInts plainA plainB =
  let withInt = pure . EPrim (getExprAnnotation plainA) . PInt
      getInt exp' = case exp' of
        (EPrim _ (PInt i)) -> Right i
        _ -> Left $ AdditionWithNonNumber plainA
   in case (,) <$> getInt plainA <*> getInt plainB of
        Right (a', b') -> withInt (a' + b')
        Left e -> throwError e

concatStrings :: InterpretExpr ann -> InterpretExpr ann -> InterpreterM ann (InterpretExpr ann)
concatStrings plainA plainB =
  let withStr = pure . EPrim (getExprAnnotation plainA) . PString
      getStr exp' = case exp' of
        (EPrim _ (PString s)) -> Right s
        _ -> Left $ AdditionWithNonNumber plainA
   in case (,) <$> getStr plainA <*> getStr plainB of
        Right (a', b') -> withStr (a' <> b')
        Left e -> throwError e
