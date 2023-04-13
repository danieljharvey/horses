{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calc.Typecheck.Elaborate (elaborate, elaborateFunction, elaborateModule) where

import Calc.ExprUtils
import Calc.PatternUtils
import Calc.TypeUtils
import Calc.Typecheck.Error
import Calc.Typecheck.Types
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Module
import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import Control.Monad (when, zipWithM)
import Calc.Utils
import Control.Monad.Except
import Data.Bifunctor (second)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

elaborateModule ::
  forall ann.
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule (Module {mdFunctions, mdExpr}) = runTypecheckM (TypecheckEnv mempty) $ do
  fns <-
    traverse
      ( \fn -> do
          elabFn <- elaborateFunction fn
          storeFunction (fnFunctionName elabFn) (fnAnn elabFn)
          pure elabFn
      )
      mdFunctions

  Module fns <$> infer mdExpr

elaborateFunction ::
  Function ann ->
  TypecheckM ann (Function (Type ann))
elaborateFunction (Function ann args name expr) = do
  exprA <- withFunctionArgs args (infer expr)
  let argsA = fmap (second (\ty -> fmap (const ty) ty)) args
  let tyFn = TFunction ann (snd <$> args) (getOuterAnnotation exprA)
  pure (Function tyFn argsA name exprA)

elaborate :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
elaborate = runTypecheckM (TypecheckEnv mempty) . infer

check :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
check ty expr = do
  exprA <- infer expr
  if void (getOuterAnnotation exprA) == void ty
    then pure (expr $> ty)
    else throwError (TypeMismatch ty (getOuterAnnotation exprA))

-- given the type of the expression in a pattern match,
-- check that the pattern makes sense with it
checkPattern ::
  ( Show ann
  ) =>
  Type ann ->
  Pattern ann ->
  TypecheckM
    ann
    ( Pattern (Type ann),
      Map Identifier (Type ann)
    )
checkPattern checkTy checkPat = do
  case (checkTy, checkPat) of
    (TTuple _ tA tRest, PTuple ann pA pRest) | length tRest == length pRest -> do
      (patA, envA) <- checkPattern tA pA
      (patRest, envRest) <- neUnzip <$> neZipWithM checkPattern tRest pRest
      let ty = TTuple ann (getPatternAnnotation patA) (getPatternAnnotation <$> patRest)
          env = envA <> mconcat (NE.toList envRest)
      pure (PTuple ty patA patRest, env)
    (ty, PVar _ ident) ->
      pure (PVar ty ident, M.singleton ident ty)
    (ty, PWildcard _) -> pure (PWildcard ty, mempty)
    (ty@(TPrim _ tPrim), PLiteral _ pPrim)
      | tPrim == typePrimFromPrim pPrim ->
          pure (PLiteral ty pPrim, mempty)
    (otherTy, otherPat) -> throwError (PatternMismatch otherPat otherTy)

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
infer (ETuple ann fstExpr restExpr) = do
  typedFst <- infer fstExpr
  typedRest <- traverse infer restExpr
  let typ =
        TTuple
          ann
          (getOuterAnnotation typedFst)
          (getOuterAnnotation <$> typedRest)
  pure $ ETuple typ typedFst typedRest
infer (EPatternMatch ann matchExpr pats) = do
  elabExpr <- infer matchExpr
  let withPair (pat, patExpr) = do
        (elabPat, newVars) <- checkPattern (getOuterAnnotation elabExpr) pat
        elabPatExpr <- withNewVars newVars (infer patExpr)
        pure (elabPat, elabPatExpr)
  elabPats <- traverse withPair pats
  let allTypes = getOuterAnnotation . snd <$> elabPats
  typ <- combineMany allTypes
  pure (EPatternMatch typ elabExpr elabPats)
infer (EApply ann fnName args) = do
  fn <- lookupFunction ann fnName
  (ty, elabArgs) <- case fn of
    TFunction _ tArgs tReturn -> do
      when
        (length args /= length tArgs)
        (throwError $ FunctionArgumentLengthMismatch ann (length tArgs) (length args))
      elabArgs <- zipWithM check tArgs args -- check each arg against type
      pure (tReturn, elabArgs)
    _ -> throwError $ NonFunctionTypeFound ann fn
  pure (EApply (ty $> ann) fnName elabArgs)
infer (EVar ann var) = do
  ty <- lookupVar ann var
  pure (EVar ty var)
infer (EInfix ann op a b) =
  inferInfix ann op a b

typePrimFromPrim :: Prim -> TypePrim
typePrimFromPrim (PInt _) = TInt
typePrimFromPrim (PBool _) = TBool

typeFromPrim :: ann -> Prim -> Type ann
typeFromPrim ann prim = TPrim ann (typePrimFromPrim prim)
