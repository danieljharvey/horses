{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module IR.ToLLVM.Helpers
  ( irPrimToLLVM,
    emptyIRState,
    storeExtern,
    storeFunction,
    addVar,
    lookupVar,
    returnType,
    lookupFunctionType,
    irTypeNeedsPointer,
    irIdentifierToLLVM,
    irTypeToLLVM,
    functionReturnType,
    irTypeMaybePointer,
    functionArgsType,
    irFunctionNameToLLVM,
    irStoreInStruct,
    getCastType,
    irVarFromPath,
    allocLocal,
    functionAndType,
    getPrintInt,
    loadFromStruct,
    storePrimInStruct,
    moveToStruct,
    callClosure,
    callWithReturnStruct,
    fromClosure,
    struct,
    pointerType,
    primFromConstructor,
  )
where

import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import GHC.Records (HasField (..))
import Helpers
import IR.IRExpr
import qualified LLVM.AST as AST
import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.Operand as Op
import LLVM.AST.Type as AST
import LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import LLVM.IRBuilder.Module
import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Monad as LLVM
import qualified Types as Smol
import Types.GetPath

-- | lookup constructor, get number for it and expected number of args
-- we'll use this to create datatype etc
primFromConstructor ::
  ( MonadState s m,
    HasField "dataTypes" s (Map Smol.TypeName (Smol.DataType ann))
  ) =>
  Smol.Constructor ->
  m Smol.Prim
primFromConstructor constructor = do
  dt <- lookupConstructor constructor
  let i = getConstructorNumber dt constructor
  pure (Smol.PInt i)

-- | lookup constructor, get number for it and expected number of args
-- we'll use this to create datatype etc
lookupConstructor ::
  ( MonadState s m,
    HasField "dataTypes" s (Map Smol.TypeName (Smol.DataType ann))
  ) =>
  Smol.Constructor ->
  m (Smol.DataType ann)
lookupConstructor constructor = do
  maybeDt <-
    gets
      ( mapFind
          ( \dt@(Smol.DataType _ _ constructors) ->
              (,) dt <$> M.lookup constructor constructors
          )
          . getField @"dataTypes"
      )
  case maybeDt of
    Just (dt, _) -> pure dt
    Nothing -> error "cant find, what the hell man"

getConstructorNumber :: Smol.DataType ann -> Smol.Constructor -> Integer
getConstructorNumber (Smol.DataType _ _ constructors) constructor =
  case M.lookup constructor (mapToNumbered constructors) of
    Just i -> i
    Nothing -> error "blah"

-- create function and it's type at once
functionAndType ::
  MonadModuleBuilder m =>
  AST.Name ->
  [(Type, ParameterName)] ->
  Type ->
  ([Op.Operand] -> L.IRBuilderT m ()) ->
  m (Op.Operand, AST.Type)
functionAndType name fnArgs tyReturn body = do
  fn <- function name fnArgs tyReturn body
  let ty =
        AST.FunctionType
          tyReturn
          (fst <$> fnArgs)
          False
  pure (fn, ty)

getPrintInt :: (L.MonadModuleBuilder m) => m Op.Operand
getPrintInt = extern "printint" [AST.i32] AST.i32

-- | given a pointer to a struct, get the value at `index`
loadFromStruct ::
  (L.MonadIRBuilder m, L.MonadModuleBuilder m) =>
  Op.Operand ->
  [Integer] ->
  m Op.Operand
loadFromStruct struct' indexes = do
  -- get pointer to slot `i`
  slot1 <- LLVM.gep struct' $ C.int32 <$> ([0] <> indexes)
  -- load value
  LLVM.load slot1 0

storePrimInStruct ::
  (L.MonadIRBuilder m, L.MonadModuleBuilder m) =>
  Op.Operand ->
  [Integer] ->
  Op.Operand ->
  m ()
storePrimInStruct struct' indexes a = do
  -- get pointer to element
  slot1 <- LLVM.gep struct' $ C.int32 <$> ([0] <> indexes)
  -- store a in slot1
  LLVM.store slot1 0 a

-- given some data in a structure, put it in another struct
moveToStruct ::
  ( L.MonadModuleBuilder m,
    L.MonadIRBuilder m
  ) =>
  Op.Operand ->
  Op.Operand ->
  m ()
moveToStruct fromStruct toStruct = do
  input <- LLVM.load fromStruct 0
  LLVM.store toStruct 0 input

callClosure ::
  ( L.MonadModuleBuilder m,
    L.MonadIRBuilder m
  ) =>
  Op.Operand ->
  Op.Operand ->
  m Op.Operand
callClosure opFunc opArg = do
  -- get fn pt and env
  (fn, env) <- fromClosure opFunc

  -- call fn with env + arg
  LLVM.call
    fn
    [ (opArg, []),
      (env, [])
    ]

-- | call a function that returns a struct, initialising a struct for it to
-- fill, then passing that to the function
callWithReturnStruct ::
  L.MonadIRBuilder m =>
  Op.Operand ->
  Type ->
  [Op.Operand] ->
  m Op.Operand
callWithReturnStruct fn structType fnArgs = do
  retStruct <- allocLocal "struct-return" structType

  let allArgs = (,[]) <$> (fnArgs <> [retStruct])

  _ <- LLVM.call fn allArgs

  pure retStruct

struct :: [AST.Type] -> AST.Type
struct =
  AST.StructureType False

pointerType :: AST.Type -> AST.Type
pointerType ty =
  AST.PointerType ty (AST.AddrSpace 0)

allocLocal ::
  (L.MonadIRBuilder m) =>
  String ->
  AST.Type ->
  m Op.Operand
allocLocal label ty =
  LLVM.alloca ty Nothing 0 `L.named` fromString label

-- | get fn and environment from closure for calling
fromClosure ::
  (L.MonadIRBuilder m, L.MonadModuleBuilder m) =>
  Op.Operand ->
  m (Op.Operand, Op.Operand)
fromClosure closure = do
  -- get fn pt
  fn <- loadFromStruct closure [0]

  -- get pointer to env
  envAddress <- LLVM.gep closure [C.int32 0, C.int32 1]

  pure (fn, envAddress)

emptyIRState :: IRState
emptyIRState = IRState mempty mempty

storeExtern :: (MonadState IRState m) => IRExtern -> m ()
storeExtern ext@(IRExtern name _ _) =
  modify (\s -> s {irFunctions = irFunctions s <> M.singleton name (Right ext)})

storeFunction :: (MonadState IRState m) => IRFunction -> m ()
storeFunction fn@(IRFunction name _ _ _) =
  modify (\s -> s {irFunctions = irFunctions s <> M.singleton name (Left fn)})

addVar :: (MonadState IRState m) => IRIdentifier -> LLVM.Operand -> m ()
addVar ident op =
  modify (\s -> s {irVars = irVars s <> M.singleton ident op})

lookupVar :: (MonadState IRState m) => IRIdentifier -> m LLVM.Operand
lookupVar ident =
  gets (M.lookup ident . irVars) >>= \case
    Just op -> pure op
    Nothing -> error $ "could not find " <> show ident

returnType :: IRType -> ([IRType], IRType)
returnType (IRFunctionType args ret) = (args, ret)
returnType (IRStruct [IRPointer (IRFunctionType args ret), _]) = (args, ret)
returnType other = error ("non-function " <> show other)

lookupFunctionType :: (MonadState IRState m) => IRFunctionName -> m LLVM.Type
lookupFunctionType fnName =
  gets (M.lookup fnName . irFunctions) >>= \case
    Just (Right (IRExtern _ eArgs eRet)) ->
      pure $ LLVM.FunctionType (irTypeToLLVM eRet) (irTypeToLLVM <$> eArgs) False
    Just (Left (IRFunction _ fArgs fRet _)) -> do
      let llRet =
            if irTypeNeedsPointer fRet
              then LLVM.void
              else irTypeToLLVM fRet
          llArgs = irTypeMaybePointer . fst <$> fArgs <> [(fRet, "sRet") | irTypeNeedsPointer fRet]
      pure $ LLVM.FunctionType llRet llArgs False
    Nothing -> do
      funcs <- gets irFunctions
      error $ "lookupFunctionType: could not find " <> show fnName <> " in " <> show funcs

irTypeNeedsPointer :: IRType -> Bool
irTypeNeedsPointer (IRStruct _) = True
irTypeNeedsPointer _ = False

irIdentifierToLLVM :: IsString a => IRIdentifier -> a
irIdentifierToLLVM (IRIdentifier s) = fromString s

irPrimToLLVM :: IRPrim -> LLVM.Operand
irPrimToLLVM (IRPrimInt32 i) = LLVM.int32 i
irPrimToLLVM (IRPrimInt2 False) = LLVM.bit 0
irPrimToLLVM (IRPrimInt2 True) = LLVM.bit 1

irTypeToLLVM :: IRType -> LLVM.Type
irTypeToLLVM IRInt32 = LLVM.i32
irTypeToLLVM IRInt2 = LLVM.i1
irTypeToLLVM (IRArray size inner) = LLVM.ArrayType size (irTypeToLLVM inner)
irTypeToLLVM (IRStruct bits) =
  LLVM.StructureType False (irTypeToLLVM <$> bits)
irTypeToLLVM (IRPointer target) =
  LLVM.PointerType (irTypeToLLVM target) (LLVM.AddrSpace 0)
irTypeToLLVM (IRFunctionType tyArgs tyRet) =
  LLVM.FunctionType (functionReturnType tyRet) (functionArgsType tyRet tyArgs) False

functionReturnType :: IRType -> LLVM.Type
functionReturnType irType =
  if irTypeNeedsPointer irType
    then LLVM.void
    else irTypeToLLVM irType

irTypeMaybePointer :: IRType -> LLVM.Type
irTypeMaybePointer ty =
  if irTypeNeedsPointer ty
    then irTypeToLLVM (IRPointer ty)
    else irTypeToLLVM ty

functionArgsType :: IRType -> [IRType] -> [LLVM.Type]
functionArgsType fnRet fnArgs =
  irTypeMaybePointer <$> (fnArgs <> [fnRet | irTypeNeedsPointer fnRet])

irFunctionNameToLLVM :: IRFunctionName -> LLVM.Name
irFunctionNameToLLVM (IRFunctionName name) = fromString name

irStoreInStruct ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) =>
  IRType ->
  LLVM.Operand ->
  [Integer] ->
  LLVM.Operand ->
  m ()
irStoreInStruct fromTy toStruct indexes from = do
  input <-
    if irTypeNeedsPointer fromTy
      then LLVM.load from 0
      else pure from
  -- get pointer to element
  slot1 <- LLVM.gep toStruct $ LLVM.int32 <$> ([0] <> indexes)
  -- store a in slot1
  LLVM.store slot1 0 input

-- when using `bitcast` we need to cast to a PointerType for structs
getCastType :: IRType -> IRType
getCastType irType =
  if irTypeNeedsPointer irType
    then IRPointer irType
    else irType

-- given a path into a struct, name it!
irVarFromPath ::
  ( MonadState IRState m,
    LLVM.MonadModuleBuilder m,
    LLVM.MonadIRBuilder m
  ) =>
  LLVM.Operand ->
  IRIdentifier ->
  GetPath ->
  m ()
irVarFromPath llExpr ident (StructPath as) = do
  val <- loadFromStruct llExpr (NE.toList as)
  addVar ident val
irVarFromPath llExpr ident ValuePath = do
  addVar ident llExpr
