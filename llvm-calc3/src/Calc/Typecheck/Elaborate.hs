{-# LANGUAGE DerivingStrategies #-}

module Calc.Typecheck.Elaborate (elaborate, elaborateFunction, elaborateModule) where

import Calc.ExprUtils
import Calc.TypeUtils
import Calc.Typecheck.Error
import Calc.Typecheck.Types
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Module
import Calc.Types.Prim
import Calc.Types.Type
import Control.Monad.Except
import Data.Bifunctor (second)
import Data.Functor

elaborateModule ::
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule _ = error "sdfsdjklfjkldsfjklsdfjkldsfjkl"

elaborateFunction ::
  Function ann ->
  Either (TypeError ann) (Function (Type ann))
elaborateFunction (Function ann args name expr) =
  runTypecheckM mempty $ do
    exprA <- withFunctionArgs args (infer expr)
    let argsA = fmap (second (\ty -> fmap (const ty) ty)) args
    let tyFn = TFunction ann (snd <$> args) (getOuterAnnotation exprA)
    pure (Function tyFn argsA name exprA)

elaborate :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
elaborate = runTypecheckM mempty . infer

check :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
check ty expr = do
  exprA <- infer expr
  if void (getOuterAnnotation exprA) == void ty
    then pure (expr $> ty)
    else throwError (TypeMismatch ty (getOuterAnnotation exprA))

inferIf ::
  ann ->
  Expr ann ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
inferIf ann predExpr thenExpr elseExpr = do
  predA <- infer predExpr
  case getOuterAnnotation predA of
    (TPrim _ TBool) -> pure ()
    otherType -> throwError (PredicateIsNotBoolean ann otherType)
  thenA <- infer thenExpr
  elseA <- check (getOuterAnnotation thenA) elseExpr
  pure (EIf (getOuterAnnotation elseA) predA thenA elseA)

inferInfix ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
inferInfix ann OpEquals a b = do
  elabA <- infer a
  elabB <- infer b
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ tA, TPrim _ tB)
      | tA == tB ->
          -- if the types are the same, then great! it's a bool!
          pure (TPrim ann TBool)
    (otherA, otherB) ->
      -- otherwise, error!
      throwError (TypeMismatch otherA otherB)
  pure (EInfix ty OpEquals elabA elabB)
inferInfix ann op a b = do
  elabA <- infer a
  elabB <- infer b
  -- all the other infix operators need to be Int -> Int -> Int
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ TInt, TPrim _ TInt) ->
      -- if the types are the same, then great! it's an int!
      pure (TPrim ann TInt)
    (TPrim _ TInt, other) ->
      throwError
        ( InfixTypeMismatch
            op
            [ ( TPrim (getOuterTypeAnnotation other) TInt,
                other
              )
            ]
        )
    (other, TPrim _ TInt) ->
      throwError
        ( InfixTypeMismatch
            op
            [ ( TPrim (getOuterTypeAnnotation other) TInt,
                other
              )
            ]
        )
    (otherA, otherB) ->
      -- otherwise, error!
      throwError
        ( InfixTypeMismatch
            op
            [ (TPrim (getOuterTypeAnnotation otherA) TInt, otherA),
              (TPrim (getOuterTypeAnnotation otherB) TInt, otherB)
            ]
        )
  pure (EInfix ty op elabA elabB)

infer :: Expr ann -> TypecheckM ann (Expr (Type ann))
infer (EPrim ann prim) =
  pure (EPrim (typeFromPrim ann prim) prim)
infer (EIf ann predExpr thenExpr elseExpr) =
  inferIf ann predExpr thenExpr elseExpr
infer (EVar ann var) = do
  ty <- lookupVar ann var
  pure (EVar ty var)
infer (EInfix ann op a b) =
  inferInfix ann op a b

typeFromPrim :: ann -> Prim -> Type ann
typeFromPrim ann (PInt _) = TPrim ann TInt
typeFromPrim ann (PBool _) = TPrim ann TBool
