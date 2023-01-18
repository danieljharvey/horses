{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.IR.FromExpr.Expr
  ( irFromExpr,
    fromExpr,
    FromExprState (..),
    getConstructorNumber,
  )
where

import Control.Monad.Identity
import Control.Monad.State
import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Smol.Core.Helpers
import Smol.Core.IR.FromExpr.DataTypes
import qualified Smol.Core.IR.FromExpr.Helpers as Compile
import Smol.Core.IR.FromExpr.Pattern
import Smol.Core.IR.FromExpr.Type
import Smol.Core.IR.FromExpr.Types
import Smol.Core.IR.IRExpr
import Smol.Core.Typecheck (builtInTypes, flattenConstructorApplication)
import Smol.Core.Typecheck.Shared (getExprAnnotation)
import Smol.Core.Types.Constructor
import Smol.Core.Types.Expr
import Smol.Core.Types.GetPath
import Smol.Core.Types.Identifier
import Smol.Core.Types.PatternPredicate
import Smol.Core.Types.Prim
import Smol.Core.Types.Type

irPrintInt :: IRModulePart
irPrintInt =
  IRExternDef
    ( IRExtern
        { ireName = "printint",
          ireArgs = [IRInt32],
          ireReturn = IRInt32
        }
    )

irPrintBool :: IRModulePart
irPrintBool =
  IRExternDef
    ( IRExtern
        { ireName = "printbool",
          ireArgs = [IRInt2],
          ireReturn = IRInt32
        }
    )

getPrinter :: (Show ann) => Type ann -> IRModulePart
getPrinter (TPrim _ TPInt) = irPrintInt
getPrinter (TPrim _ TPNat) = irPrintInt
getPrinter (TPrim _ TPBool) = irPrintBool
getPrinter (TLiteral _ (TLBool _)) = irPrintBool
getPrinter (TLiteral _ (TLInt _)) = irPrintInt
getPrinter other = error ("could not find a printer for type " <> show other)

getPrintFuncName :: (Show ann) => Type ann -> IRFunctionName
getPrintFuncName ty =
  case getPrinter ty of
    (IRExternDef (IRExtern n _ _)) -> n
    other -> error (show other)

getPrintFuncType :: (Show ann) => Type ann -> IRType
getPrintFuncType ty =
  case getPrinter ty of
    (IRExternDef (IRExtern _ fnArgs fnReturn)) -> IRFunctionType fnArgs fnReturn
    other -> error (show other)

getFreshName :: (MonadState (FromExprState ann) m) => String -> m String
getFreshName prefix = do
  current <- gets freshInt
  modify (\s -> s {freshInt = current + 1})
  pure (prefix <> show current)

getFreshFunctionName :: (MonadState (FromExprState ann) m) => m IRFunctionName
getFreshFunctionName = IRFunctionName <$> getFreshName "function"

getFreshClosureName :: (MonadState (FromExprState ann) m) => m IRIdentifier
getFreshClosureName = IRIdentifier <$> getFreshName "closure"

addVar ::
  (MonadState (FromExprState ann) m) =>
  IRIdentifier ->
  IRExpr ->
  m ()
addVar ident expr =
  modify (\s -> s {vars = vars s <> M.singleton ident expr})

{-
lookupVar ::
  (MonadState (FromExprState ann) m) =>
  IRIdentifier ->
  m IRExpr
lookupVar ident = do
  maybeExpr <- gets (M.lookup ident . vars)
  case maybeExpr of
    Just a -> pure a
    _ -> error $ "could not find var " <> show ident
-}

-- in which we turn our higher level language into the middle level silly
-- language
-- we should create the main function, which always prints its result
-- a side effect will be that it adds IRModuleParts to the state, these are all
-- combined
irFromExpr ::
  (Show ann, Monoid ann) =>
  IdentityExpr (Type ann) ->
  IRModule
irFromExpr expr =
  IRModule $
    [getPrinter (getExprAnnotation expr)]
      <> modulePartsFromExpr expr

fromPrim :: Prim -> IRPrim
fromPrim (PInt i) = IRPrimInt32 i
fromPrim (PNat i) = IRPrimInt32 (fromIntegral i)
fromPrim (PBool b) = IRPrimInt2 b
fromPrim PUnit = IRPrimInt2 False -- Unit is represented the same as False

fromInfix ::
  (Show ann, MonadState (FromExprState ann) m) =>
  Op ->
  IdentityExpr (Type ann) ->
  IdentityExpr (Type ann) ->
  m IRExpr
fromInfix op a b = do
  let irOp = case op of
        OpAdd -> IRAdd
        OpEquals -> IREquals
  IRInfix irOp <$> fromExpr a <*> fromExpr b

functionReturnType :: IRType -> ([IRType], IRType)
functionReturnType (IRStruct [IRPointer (IRFunctionType args ret), _]) =
  (args, ret)
functionReturnType other = error ("non-function " <> show other)

fromExpr ::
  (Show ann, MonadState (FromExprState ann) m) =>
  IdentityExpr (Type ann) ->
  m IRExpr
fromExpr (EPrim _ prim) = pure (IRPrim $ fromPrim prim)
fromExpr (EInfix _ op a b) = fromInfix op a b
fromExpr (EAnn _ _ inner) = fromExpr inner
fromExpr (EIf ty predExpr thenExpr elseExpr) = do
  irPred <- fromExpr predExpr
  irThen <- fromExpr thenExpr
  irElse <- fromExpr elseExpr
  responseType <- fromType ty
  pure $
    IRMatch
      irPred
      responseType
      ( NE.fromList
          [ IRMatchCase
              { irmcType = IRInt2,
                irmcPatternPredicate = [PathEquals ValuePath (IRPrimInt2 True)],
                irmcGetPath = mempty,
                irmcExpr = irThen
              },
            IRMatchCase
              { irmcType = IRInt2,
                irmcPatternPredicate = [PathEquals ValuePath (IRPrimInt2 False)],
                irmcGetPath = mempty,
                irmcExpr = irElse
              }
          ]
      )
fromExpr (EPatternMatch ty matchExpr pats) = do
  irMatch <- fromExpr matchExpr
  let withPat (p, pExpr) = do
        irExpr <- fromExpr pExpr
        preds <- predicatesFromPattern fromPrim p
        destructured <- destructurePattern fromIdentifier p
        dt <- patternTypeInMemory p
        pure $
          IRMatchCase
            { irmcType = fromDataTypeInMemory dt,
              irmcPatternPredicate = preds,
              irmcGetPath = destructured,
              irmcExpr = irExpr
            }
  irPats <- traverse withPat pats
  responseType <- fromType ty
  pure $
    IRMatch
      irMatch
      responseType
      irPats
fromExpr (ELambda ty ident body) =
  lambdaFromExpr ty (runIdentity ident) body
fromExpr (EApp ty fn val) =
  appFromExpr ty fn val
fromExpr (EVar _ var) =
  pure $ IRVar (fromIdentifier (runIdentity var))
fromExpr (ETuple ty tHead tTail) = do
  statements <-
    traverseInd
      ( \expr i -> do
          irExpr <- fromExpr expr
          exprType <- fromType (getExprAnnotation expr)
          pure $ IRSetTo [i] exprType irExpr
      )
      ([tHead] <> NE.toList tTail)
  structType <- fromType ty
  pure $
    IRInitialiseDataType
      (IRAlloc structType)
      structType
      structType
      statements
fromExpr (ELet _ ident expr body) = do
  irExpr <- fromExpr expr
  addVar (fromIdentifier (runIdentity ident)) irExpr -- remember pls
  irBody <- fromExpr body
  pure (IRLet (fromIdentifier (runIdentity ident)) irExpr irBody)
fromExpr (EConstructor ty constructor) = do
  tyResult <- Compile.flattenConstructorType ty
  case tyResult of
    -- genuine enum, return number
    (_typeName, []) -> IRPrim <$> getConstructorNumber (runIdentity constructor)
    (_typeName, _) -> do
      (structType, specificStructType) <-
        bimap
          fromDataTypeInMemory
          fromDataTypeInMemory
          <$> constructorTypeInMemory ty (runIdentity constructor)

      -- get number for constructor
      consNum <- getConstructorNumber (runIdentity constructor)

      let setConsNum = IRSetTo [0] IRInt32 (IRPrim consNum)

      pure $
        IRInitialiseDataType
          (IRAlloc structType)
          specificStructType
          structType
          [setConsNum]
fromExpr expr = error ("fuck: " <> show expr)

-- | given an env type, put all it's items in scope
-- replaces "a" with a reference it's position in scope
bindingsFromEnv :: Map Identifier (Type ann) -> IRExpr -> IRExpr
bindingsFromEnv env inner =
  foldr
    ( \(ident, i) irExpr ->
        swapVar (fromIdentifier ident) (IRStructPath [i] (IRVar "env")) irExpr
    )
    inner
    (zip (M.keys env) [0 ..])

swapVar :: IRIdentifier -> IRExpr -> IRExpr -> IRExpr
swapVar target replace =
  go
  where
    go (IRVar a) | a == target = replace
    go other = mapIRExpr go other

mapIRExpr :: (IRExpr -> IRExpr) -> IRExpr -> IRExpr
mapIRExpr _ (IRVar a) = IRVar a
mapIRExpr _ (IRAlloc ty) = IRAlloc ty
mapIRExpr _ (IRPrim p) = IRPrim p
mapIRExpr f (IRInfix op a b) = IRInfix op (f a) (f b)
mapIRExpr f (IRApply ty fn arg) = IRApply ty (f fn) (f <$> arg)
mapIRExpr f (IRLet ident expr rest) =
  IRLet ident (f expr) (f rest)
mapIRExpr f (IRStructPath as var) =
  IRStructPath as (f var)
mapIRExpr _ (IRFuncPointer p) = IRFuncPointer p
mapIRExpr f (IRMatch expr ty pats) =
  IRMatch (f expr) ty ((\(IRMatchCase a b c irExpr) -> IRMatchCase a b c (f irExpr)) <$> pats)
mapIRExpr f (IRStatements as rest) =
  IRStatements as (f rest)
mapIRExpr f (IRPointerTo a b) =
  IRPointerTo a (f b)
mapIRExpr f (IRInitialiseDataType input a b args) =
  let mapSetTo (IRSetTo path ty expr) = IRSetTo path ty (f expr)
   in IRInitialiseDataType (f input) a b (mapSetTo <$> args)

lambdaFromExpr ::
  ( MonadState (FromExprState ann) m,
    Show ann
  ) =>
  Type ann ->
  Identifier ->
  IdentityExpr (Type ann) ->
  m IRExpr
lambdaFromExpr ty ident body = do
  irType <- fromType ty
  let (argTypes, retType) = functionReturnType irType
  let argType = case argTypes of
        (a : _) -> a
        _ -> error "why don't we have any args to this function?"
  let envArgs = case ty of
        TFunc _ env _ _ -> env
        _ -> error "type is not lambda wtf"

  funcName <- getFreshFunctionName

  irBody <- fromExpr body
  envType <- typeFromEnv envArgs

  modulePart <- do
    pure
      ( IRFunctionDef
          ( IRFunction
              { irfName = funcName,
                irfArgs = [(argType, fromIdentifier ident), (envType, "env")],
                irfReturn = retType,
                irfBody =
                  [ IRRet retType (bindingsFromEnv envArgs irBody)
                  ]
              }
          )
      )

  pushModulePart modulePart

  let functionType = IRFunctionType [argType, envType] retType
      closureType = IRStruct [IRPointer functionType, envType]

  envStatements <- structFromEnv envArgs

  pure $
    IRInitialiseDataType
      (IRAlloc closureType)
      closureType
      closureType
      ( [ IRSetTo
            [0]
            (IRPointer functionType)
            (IRFuncPointer funcName)
        ]
          <> envStatements
      )

-- given an `env` value, capture all the vars from the environment to put in
-- the closure
structFromEnv ::
  ( MonadState (FromExprState ann) m,
    Show ann
  ) =>
  Map Identifier (Type ann) ->
  m [IRSetTo]
structFromEnv env =
  traverseInd
    ( \(ident, ty) i -> do
        irType <- fromType ty
        let irVal = IRVar (fromIdentifier ident)
        pure (IRSetTo [1, i] irType irVal)
    )
    (M.toList env)

-- | applying `1` to `Just`, in the literal `Just 1` for instance
constructorAppFromExpr ::
  ( MonadState (FromExprState ann) m,
    Show ann
  ) =>
  Type ann ->
  Constructor ->
  [IdentityExpr (Type ann)] ->
  m IRExpr
constructorAppFromExpr ty constructor cnArgs = do
  -- the constructor case, build up everything we need pls
  (structType, specificStructType) <-
    bimap
      fromDataTypeInMemory
      fromDataTypeInMemory
      <$> constructorTypeInMemory ty constructor

  -- get number for constructor
  consNum <- getConstructorNumber constructor

  let setConsNum = IRSetTo [0] IRInt32 (IRPrim consNum)

  statements <-
    traverseInd
      ( \expr i -> do
          irExpr <- fromExpr expr
          exprType <- fromType (getExprAnnotation expr)
          pure $
            IRSetTo
              [i + 1]
              exprType
              irExpr
      )
      cnArgs

  pure $
    IRInitialiseDataType
      (IRAlloc structType)
      specificStructType
      structType
      ([setConsNum] <> statements)

-- | application could be function application or constructor application
-- first, we need to deal with nested `app` around a constructor and flatten
-- that into something ok
appFromExpr ::
  (Show ann, MonadState (FromExprState ann) m) =>
  Type ann ->
  IdentityExpr (Type ann) ->
  IdentityExpr (Type ann) ->
  m IRExpr
appFromExpr ty fn val = do
  case flattenConstructorApplication (EApp ty fn val) of
    Just (constructor, cnArgs) ->
      constructorAppFromExpr ty (runIdentity constructor) cnArgs
    Nothing -> do
      -- regular function application (`id True` for instance)
      irFn <- fromExpr fn
      irVal <- fromExpr val
      fnType <- fromType (getExprAnnotation fn)
      closureName <- getFreshClosureName

      -- arguably we could look into trashing the env
      -- where it's empty but for now let's keep this easier
      pure
        ( IRLet
            closureName
            irFn
            ( IRApply
                fnType
                (IRStructPath [0] irFn)
                [ irVal,
                  IRPointerTo [1] (IRVar closureName)
                ]
            )
        )

fromIdentifier :: Identifier -> IRIdentifier
fromIdentifier (Identifier ident) = IRIdentifier (T.unpack ident)

pushModulePart :: (MonadState (FromExprState ann) m) => IRModulePart -> m ()
pushModulePart part =
  modify (\s -> s {fesModuleParts = fesModuleParts s <> [part]})

-- | given an expr, return the `main` function, as well as adding any extra
-- module Core.parts to the State
modulePartsFromExpr ::
  (Show ann, Monoid ann) =>
  IdentityExpr (Type ann) ->
  [IRModulePart]
modulePartsFromExpr expr =
  let (mainExpr, FromExprState otherParts _ _ _) =
        runState (fromExpr expr) (FromExprState mempty builtInTypes 1 mempty)
      printFuncName = getPrintFuncName (getExprAnnotation expr)
      printFuncType = getPrintFuncType (getExprAnnotation expr)
   in otherParts
        <> [ IRFunctionDef
               ( IRFunction
                   { irfName = "main",
                     irfArgs = [],
                     irfReturn = IRInt32,
                     irfBody =
                       [ IRDiscard (IRApply printFuncType (IRFuncPointer printFuncName) [mainExpr]),
                         IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                       ]
                   }
               )
           ]

getConstructorNumber ::
  (MonadState (FromExprState ann) m) =>
  Constructor ->
  m IRPrim
getConstructorNumber =
  fmap fromPrim . Compile.primFromConstructor
