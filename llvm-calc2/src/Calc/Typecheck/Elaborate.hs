{-# LANGUAGE DerivingStrategies #-}
module Calc.Typecheck.Elaborate (elaborate) where

import Calc.Types.Expr
import Calc.Types.Type
import Calc.Types.Prim
import Calc.ExprUtils
import Control.Monad.Except

data TypeError ann = TypeMismatch (Type ann) (Type ann)
  deriving stock (Eq,Ord,Show)

elaborate :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
elaborate = infer

infer :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
infer (EPrim ann prim) =
  pure (EPrim (typeFromPrim ann prim) prim)
infer (EInfix ann OpEquals a b) = do
  elabA <- infer a
  elabB <- infer b
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ tA, TPrim _ tB) | tA == tB ->
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
