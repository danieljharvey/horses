{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Calc.Compile.ToLLVM (moduleToLLVM, OutputError (..), OutputState (..)) where

import Calc.ExprUtils
import Calc.TypeUtils
import Calc.Types
import Control.Monad.Except
import Control.Monad.Fix (MonadFix)
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String (fromString)
import qualified Data.Text as T
import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM

data OutputState = OutputState
  { osFunctions :: Map FunctionName LLVM.Operand,
    osVars :: Map Identifier LLVM.Operand
  }

data OutputError
  = CantFindVar Identifier
  | CantFindFunction FunctionName
  | NonFunctionType (Type ())
  deriving stock (Eq, Ord, Show)

lookupFunction ::
  ( MonadError OutputError m,
    MonadState OutputState m
  ) =>
  FunctionName ->
  m LLVM.Operand
lookupFunction fnName = do
  maybeOp <- gets (M.lookup fnName . osFunctions)
  case maybeOp of
    Just found -> pure found
    Nothing -> throwError (CantFindFunction fnName)

saveFunction ::
  (MonadState OutputState m) =>
  FunctionName ->
  LLVM.Operand ->
  m ()
saveFunction fnName operand =
  modify
    ( \os ->
        os {osFunctions = M.singleton fnName operand <> osFunctions os}
    )

saveArgs :: (MonadState OutputState m) => Map Identifier LLVM.Operand -> m ()
saveArgs args =
  modify
    ( \os ->
        os {osVars = args <> osVars os}
    )

lookupArg :: (MonadState OutputState m, MonadError OutputError m) => Identifier -> m LLVM.Operand
lookupArg identifier = do
  maybeArg <- gets (M.lookup identifier . osVars)
  case maybeArg of
    Just op -> pure op
    Nothing -> throwError (CantFindVar identifier)

-- import the correct output function from our standard library
-- depending on the output type of our expression
printFunction :: (LLVM.MonadModuleBuilder m) => Type ann -> m LLVM.Operand
printFunction (TPrim _ TInt) = LLVM.extern "printint" [LLVM.i32] LLVM.void
printFunction (TPrim _ TBool) = LLVM.extern "printbool" [LLVM.i1] LLVM.void
printFunction (TTuple {}) = error "printFunction TTuple"
printFunction (TFunction _ _ tyRet) = printFunction tyRet -- maybe this should be an error instead

-- | given our `Module` type, turn it into an LLVM module
moduleToLLVM :: Module (Type ann) -> Either OutputError LLVM.Module
moduleToLLVM (Module {mdExpr = expr, mdFunctions}) =
  flip evalStateT (OutputState mempty mempty) $ LLVM.buildModuleT "example" $ do
    -- get the printing function for our `expr`'s return type
    printFn <- printFunction (getOuterAnnotation expr)

    -- create all our functions
    traverse_ functionToLLVM mdFunctions

    -- create a function called `main` that will be the entry point to our
    -- program
    LLVM.function "main" [] LLVM.i32 $ \_ -> do
      -- build the LLVM AST for our expression
      ourExpression <- exprToLLVM expr

      -- print our result to stdout
      _ <- LLVM.call printFn [(ourExpression, [])]

      -- return success exit code of `0`
      LLVM.ret (LLVM.int32 0)

functionArgToLLVM ::
  (ArgumentName, Type (Type ann)) ->
  (LLVM.Type, LLVM.ParameterName)
functionArgToLLVM (ArgumentName argName, ty) =
  let llvmType = typeToLLVM (getOuterTypeAnnotation ty)
      paramName = LLVM.ParameterName (fromString (T.unpack argName))
   in (llvmType, paramName)

functionToLLVM ::
  ( LLVM.MonadModuleBuilder m,
    MonadFix m,
    MonadState OutputState m,
    MonadError OutputError m
  ) =>
  Function (Type ann) ->
  m ()
functionToLLVM (Function {fnAnn, fnFunctionName, fnBody, fnArgs}) = do
  retType <- case fnAnn of
    TFunction _ _ tyRet -> pure $ typeToLLVM tyRet
    _ -> throwError (NonFunctionType (fnAnn $> ()))

  let argTypes = functionArgToLLVM <$> fnArgs
      functionName = functionNameToLLVM fnFunctionName

  llvmFunction <- LLVM.function functionName argTypes retType $ \args -> do
    saveArgs
      ( M.fromList $
          zipWith
            ( \(ArgumentName argName, _) arg ->
                (Identifier argName, arg)
            )
            fnArgs
            args
      )

    -- build the LLVM AST for our expression
    ourExpression <- exprToLLVM fnBody

    LLVM.ret ourExpression
  saveFunction fnFunctionName llvmFunction

functionNameToLLVM :: FunctionName -> LLVM.Name
functionNameToLLVM (FunctionName fnName) =
  LLVM.Name (fromString (T.unpack fnName))

typeToLLVM :: Type ann -> LLVM.Type
typeToLLVM (TPrim _ TBool) = LLVM.i1
typeToLLVM (TPrim _ TInt) = LLVM.i32
typeToLLVM TTuple {} = error "typeToLLVM TTuple"
typeToLLVM (TFunction _ tyArgs tyRet) =
  LLVM.FunctionType (typeToLLVM tyRet) (typeToLLVM <$> tyArgs) False

ifToLLVM ::
  ( MonadFix m,
    LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m,
    MonadState OutputState m,
    MonadError OutputError m
  ) =>
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
  ( MonadState OutputState m,
    LLVM.MonadModuleBuilder m,
    LLVM.MonadIRBuilder m,
    MonadError OutputError m,
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

exprToLLVM ::
  ( LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m,
    MonadState OutputState m,
    MonadError OutputError m,
    MonadFix m
  ) =>
  Expr (Type ann) ->
  m LLVM.Operand
exprToLLVM (EPrim _ prim) =
  pure $ primToLLVM prim
exprToLLVM (EVar _ var) =
  lookupArg var
exprToLLVM (ETuple {}) = error "exprToLLVM ETuple"
exprToLLVM (EApply _ fnName args) = do
  irFunc <- lookupFunction fnName
  irArgs <- traverse exprToLLVM args
  LLVM.call irFunc ((,[]) <$> irArgs)
exprToLLVM (EIf tyReturn predExpr thenExpr elseExpr) =
  ifToLLVM tyReturn predExpr thenExpr elseExpr
exprToLLVM (EInfix _ op a b) =
  infixToLLVM op a b

primToLLVM :: Prim -> LLVM.Operand
primToLLVM (PInt i) = LLVM.int32 (fromIntegral i)
primToLLVM (PBool True) = LLVM.bit 1
primToLLVM (PBool False) = LLVM.bit 0
