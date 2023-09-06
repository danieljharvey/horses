{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Smol.Backend.IR.ToLLVM.ToLLVM
  ( irToLLVM,
  )
where

import Control.Monad.Fix (MonadFix)
import Control.Monad.State
  ( MonadState,
    StateT (runStateT),
  )
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.Constant as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM
import Smol.Backend.IR.IRExpr
import Smol.Backend.IR.ToLLVM.Helpers
import Smol.Backend.IR.ToLLVM.Patterns
import Smol.Core.Helpers (traverseIndNe)

irToLLVM :: IRModule -> LLVM.Module
irToLLVM (IRModule bits) =
  LLVM.buildModule "example" $ do
    runStateT (traverse_ irModulePartToLLVM bits) emptyIRState

irModulePartToLLVM ::
  ( LLVM.MonadModuleBuilder m,
    MonadFix m,
    MonadState IRState m
  ) =>
  IRModulePart ->
  m ()
irModulePartToLLVM (IRFunctionDef fn@(IRFunction fnName fnArgs fnRet fnBody)) = do
  storeFunction fn
  let allFnArgs = fnArgs <> [(IRPointer fnRet, "sRet") | irTypeNeedsPointer fnRet]
      args =
        bimap irTypeMaybePointer irIdentifierToLLVM <$> allFnArgs
      ret = functionReturnType fnRet
      name = irFunctionNameToLLVM fnName
  _ <- LLVM.function name args ret $ \inputs -> do
    -- put vars in scope
    let pairs = zip (snd <$> allFnArgs) inputs
    traverse_ (uncurry addVar) pairs
    traverse_ irStatementToLLVM fnBody
  pure ()
irModulePartToLLVM (IRExternDef ext@(IRExtern eName eArgs eRet)) = do
  storeExtern ext
  _ <- LLVM.extern (irFunctionNameToLLVM eName) (irTypeToLLVM <$> eArgs) (irTypeToLLVM eRet)
  pure ()

irStatementToLLVM ::
  ( MonadFix m,
    LLVM.MonadModuleBuilder m,
    LLVM.MonadIRBuilder m,
    MonadState IRState m
  ) =>
  IRStatement ->
  m ()
irStatementToLLVM (IRRet ty expr) =
  if irTypeNeedsPointer ty
    then do
      opExpr <- irExprToLLVM expr
      opRet <- lookupVar "sRet" -- magic strings, what could go wrong
      moveToStruct opExpr opRet -- copy the return value to 'sRet'
      LLVM.retVoid
    else irExprToLLVM expr >>= LLVM.ret
irStatementToLLVM (IRSet path tyFrom fromExp toExp) = do
  opFrom <- irExprToLLVM fromExp
  opTo <- irExprToLLVM toExp
  irStoreInStruct tyFrom opTo path opFrom
irStatementToLLVM (IRDiscard expr) = do
  _ <- irExprToLLVM expr
  pure ()

irExprToLLVM ::
  ( LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m,
    MonadState IRState m,
    MonadFix m
  ) =>
  IRExpr ->
  m LLVM.Operand
irExprToLLVM (IRPrim prim) = pure $ irPrimToLLVM prim
irExprToLLVM (IRString txt) = lookupString txt
irExprToLLVM (IRCast ty expr) = do
  irExpr <- irExprToLLVM expr
  LLVM.bitcast irExpr (irTypeToLLVM (getCastType ty))
irExprToLLVM (IRApply fnType fn fnArgs) = do
  functionConst <- irExprToLLVM fn
  let (_, tyRet) = returnType fnType
  if irTypeNeedsPointer tyRet
    then do
      retStruct <- LLVM.alloca (irTypeToLLVM tyRet) Nothing 0 `LLVM.named` "ret-struct"
      args <-
        traverse
          ( \arg -> do
              opExpr <- irExprToLLVM arg
              pure (opExpr, [])
          )
          fnArgs
      _ <- LLVM.call functionConst (args <> [(retStruct, [])])
      pure retStruct
    else do
      args <-
        traverse
          ( \arg -> do
              opExpr <- irExprToLLVM arg
              pure (opExpr, [])
          )
          fnArgs
      LLVM.call functionConst args
irExprToLLVM (IRAlloc ty) = do
  LLVM.alloca (irTypeToLLVM ty) Nothing 0
irExprToLLVM (IRStructPath path expr) = do
  llExpr <- irExprToLLVM expr
  loadFromStruct llExpr path
irExprToLLVM (IRPointerTo i expr) = do
  llExpr <- irExprToLLVM expr
  LLVM.gep llExpr $ LLVM.int32 <$> ([0] <> i) -- get pointer to item
irExprToLLVM (IRVar ident) = lookupVar ident
irExprToLLVM (IRLet ident expr body) = do
  llExpr <- irExprToLLVM expr
  addVar ident llExpr
  irExprToLLVM body
irExprToLLVM (IRInfix op a b) = irInfixToLLVM op a b
irExprToLLVM (IRStatements statements expr) = do
  traverse_ irStatementToLLVM statements
  irExprToLLVM expr
irExprToLLVM (IRInitialiseDataType input tyThis tyWhole args) = do
  llInput <- irExprToLLVM input

  -- cast to tyThis
  castInput <- LLVM.bitcast llInput (irTypeToLLVM (getCastType tyThis))

  -- set all the items inside

  let setArg (IRSetTo path ty arg) = do
        opArg <- irExprToLLVM arg
        irStoreInStruct ty castInput path opArg
  traverse_ setArg args

  -- case to tyWhole
  LLVM.bitcast castInput (irTypeToLLVM (getCastType tyWhole))
irExprToLLVM (IRMatch matchExpr tyRet matches) = mdo
  llMatch <- irExprToLLVM matchExpr
  -- make return hole
  retValue <- LLVM.alloca (irTypeToLLVM tyRet) Nothing 0

  -- create an int with the index of the matching case
  matchingInt <-
    selectToOperand
      llMatch
      irExprToLLVM
      ( createSelectTable
          ( (\match -> (irmcPatternPredicate match, irmcType match)) <$> matches
          )
      )

  -- go to that block ...
  -- block returns from all this
  let defaultLabel = snd (NE.head blocks)
  LLVM.switch matchingInt defaultLabel (NE.toList blocks)

  -- now we create a table of which thing to do, depending on value of
  -- `matchValue`
  blocks <-
    traverseIndNe
      ( \(tyCast, fetches, expr) i ->
          (,) (LLVM.Int 32 i)
            <$> irExprToLLVMBlock expr tyCast fetches mergeBlock llMatch retValue
      )
      ((\irMatchCase -> (irmcType irMatchCase, irmcGetPath irMatchCase, irmcExpr irMatchCase)) <$> matches)

  -- merge block, we always end up here
  mergeBlock <- LLVM.block `LLVM.named` "merge"
  -- return value
  LLVM.load retValue 0
irExprToLLVM (IRFuncPointer fnName) = irFuncPointerToLLVM fnName

irInfixToLLVM ::
  ( LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m,
    MonadState IRState m,
    MonadFix m
  ) =>
  IROp ->
  IRExpr ->
  IRExpr ->
  m LLVM.Operand
irInfixToLLVM IRAdd a b = do
  lhs <- irExprToLLVM a
  rhs <- irExprToLLVM b
  LLVM.add lhs rhs
irInfixToLLVM IREquals a b = do
  lhs <- irExprToLLVM a
  rhs <- irExprToLLVM b
  LLVM.icmp LLVM.EQ lhs rhs

irExprToLLVMBlock ::
  ( MonadFix m,
    LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m,
    MonadState IRState m
  ) =>
  IRExpr ->
  IRType ->
  Map IRIdentifier GetPath ->
  LLVM.Name ->
  LLVM.Operand ->
  LLVM.Operand ->
  m LLVM.Name
irExprToLLVMBlock blockExpr tyCastTo destructured mergeBlock opExpr opResult = do
  -- start a new block
  patBlock <- LLVM.block `LLVM.named` "pattern"

  -- cast opExpr to the right type
  castExpr <- LLVM.bitcast opExpr (irTypeToLLVM (getCastType tyCastTo))

  -- add vals to scope
  traverse_ (uncurry $ irVarFromPath castExpr) (M.toList destructured)

  -- run patExpr with new destructured vars
  patResult <- irExprToLLVM blockExpr

  -- store result
  LLVM.store opResult 0 patResult

  -- branch home
  LLVM.br mergeBlock

  -- return a reference to the block
  pure patBlock
