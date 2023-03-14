{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Calc.Compile.ToLLVM (toLLVM) where

import Calc.ExprUtils
import Calc.Types
import Control.Monad.Fix
import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM

-- import the correct output function from our standard library
-- depending on the output type of our expression
printFunction :: (LLVM.MonadModuleBuilder m) => Type ann -> m LLVM.Operand
printFunction (TPrim _ TInt) = LLVM.extern "printint" [LLVM.i32] LLVM.void
printFunction (TPrim _ TBool) = LLVM.extern "printbool" [LLVM.i1] LLVM.void

-- | given our `Expr` type, turn it into an LLVM module
toLLVM :: Expr (Type ann) -> LLVM.Module
toLLVM expr =
  LLVM.buildModule "example" $ do
    -- get the printing function for our `expr`'s return type
    printFn <- printFunction (getOuterAnnotation expr)

    -- create a function called `main` that will be the entry point to our
    -- program
    LLVM.function "main" [] LLVM.i32 $ \_ -> do
      -- build the LLVM AST for our expression
      ourExpression <- exprToLLVM expr

      -- print our result to stdout
      _ <- LLVM.call printFn [(ourExpression, [])]

      -- return success exit code of `0`
      LLVM.ret (LLVM.int32 0)

typeToLLVM :: Type ann -> LLVM.Type
typeToLLVM (TPrim _ TBool) = LLVM.i1
typeToLLVM (TPrim _ TInt) = LLVM.i32

exprToLLVM ::
  ( LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m,
    MonadFix m
  ) =>
  Expr (Type ann) ->
  m LLVM.Operand
exprToLLVM (EPrim _ prim) =
  pure $ primToLLVM prim
exprToLLVM (EIf tyReturn predExpr thenExpr elseExpr) =
  ifToLLVM tyReturn predExpr thenExpr elseExpr
exprToLLVM (EInfix _ op a b) =
  infixToLLVM op a b

ifToLLVM ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) =>
  Type ann ->
  Expr (Type ann) ->
  Expr (Type ann) ->
  Expr (Type ann) ->
  m LLVM.Operand
ifToLLVM tyReturn predExpr thenExpr elseExpr = mdo
  -- create IR for predicate
  irPred <- exprToLLVM predExpr

  -- make variable for return value
  irReturnValue <- LLVM.alloca (typeToLLVM tyReturn) Nothing 0

  -- this does the switching
  -- we haven't created these blocks yet but RecursiveDo lets us do this with
  -- MonadFix magic
  LLVM.condBr irPred thenBlock elseBlock

  -- create a block for the 'then` branch
  thenBlock <- LLVM.block `LLVM.named` "then"
  -- create ir for the then branch
  irThen <- exprToLLVM thenExpr
  -- store the result in irResultValue
  LLVM.store irReturnValue 0 irThen
  -- branch back to the 'done' block
  LLVM.br doneBlock

  -- create a block for the 'else' branch
  elseBlock <- LLVM.block `LLVM.named` "else"
  -- create ir for the else branch
  irElse <- exprToLLVM elseExpr
  -- store the result in irReturnValue
  LLVM.store irReturnValue 0 irElse
  -- branch back to the `done` block
  LLVM.br doneBlock

  -- create a block for 'done' that we always branch to
  doneBlock <- LLVM.block `LLVM.named` "done"
  -- load the result and return it
  LLVM.load irReturnValue 0

infixToLLVM ::
  ( LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m,
    MonadFix m
  ) =>
  Op ->
  Expr (Type ann) ->
  Expr (Type ann) ->
  m LLVM.Operand
infixToLLVM OpAdd a b = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.add lhs rhs
infixToLLVM OpSubtract a b = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.sub lhs rhs
infixToLLVM OpMultiply a b = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.mul lhs rhs
infixToLLVM OpEquals a b = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.icmp LLVM.EQ lhs rhs

primToLLVM :: Prim -> LLVM.Operand
primToLLVM (PInt i) = LLVM.int32 (fromIntegral i)
primToLLVM (PBool True) = LLVM.bit 1
primToLLVM (PBool False) = LLVM.bit 0
