{-# LANGUAGE DerivingStrategies #-}

module Calc.Typecheck.Elaborate (elaborate, TypeError (..)) where

import Calc.ExprUtils
import Calc.Types.Expr
import Calc.Types.Prim
import Calc.Types.Type
import Control.Monad.Except
import Data.Functor

data TypeError ann = TypeMismatch (Type ann) (Type ann)
  deriving stock (Eq, Ord, Show)

elaborate :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
elaborate = infer

check :: Type ann -> Expr ann -> Either (TypeError ann) (Expr (Type ann))
check ty expr = do
  exprA <- infer expr
  if void (getOuterAnnotation exprA) == void ty
    then pure (expr $> ty)
    else throwError (TypeMismatch ty (getOuterAnnotation exprA))

infer :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
infer (EPrim ann prim) =
  pure (EPrim (typeFromPrim ann prim) prim)
infer (EIf ann predExpr thenExpr elseExpr) = do
  predA <- check (TPrim ann TBool) predExpr
  thenA <- infer thenExpr
  elseA <- check (getOuterAnnotation thenA) elseExpr
  pure (EIf (getOuterAnnotation elseA) predA thenA elseA)
infer (EInfix ann OpEquals a b) = do
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
infer (EInfix ann op a b) = do
  elabA <- infer a
  elabB <- infer b
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ TInt, TPrim _ TInt) ->
      -- if the types are the same, then great! it's an int!
      pure (TPrim ann TInt)
    (otherA, otherB) ->
      -- otherwise, error!
      throwError (TypeMismatch otherA otherB)
  pure (EInfix ty op elabA elabB)

typeFromPrim :: ann -> Prim -> Type ann
typeFromPrim ann (PInt _) = TPrim ann TInt
typeFromPrim ann (PBool _) = TPrim ann TBool
