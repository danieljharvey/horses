{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Compile.ToLLVM (toLLVM) where

import Calc.Types
import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM

-- | given our `Expr` type, turn it into an LLVM module
toLLVM :: Expr ann -> LLVM.Module
toLLVM expr =
  LLVM.buildModule "example" $ do
    -- import `printint` from our standard library
    -- it takes an `i32` and returns an `i32`
    printInt <- LLVM.extern "printint" [LLVM.i32] LLVM.i32

    -- create a function called `main` that will be the entry point to our
    -- program
    LLVM.function "main" [] LLVM.i32 $ \_ -> do
      -- build the LLVM AST for our expression
      ourExpression <- exprToLLVM expr

      -- print our result to stdout
      _ <- LLVM.call printInt [(ourExpression, [])]

      -- return success exit code of `0`
      LLVM.ret (LLVM.int32 0)

exprToLLVM ::
  ( LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m
  ) =>
  Expr ann ->
  m LLVM.Operand
exprToLLVM (EPrim _ prim) = pure $ primToLLVM prim
exprToLLVM (EInfix _ OpAdd a b) = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.add lhs rhs
exprToLLVM (EInfix _ OpSubtract a b) = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.sub lhs rhs
exprToLLVM (EInfix _ OpMultiply a b) = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.mul lhs rhs
exprToLLVM (EInfix _ OpEquals a b) = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.icmp LLVM.EQ lhs rhs

primToLLVM :: Prim -> LLVM.Operand
primToLLVM (PInt i) = LLVM.int32 (fromIntegral i)
primToLLVM (PBool True) = LLVM.bit 1
primToLLVM (PBool False) = LLVM.bit 0
