{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Convert (fromExpr, toExpr) where

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import ExprUtils
import Interpreter.Types
import Types

swapOutVar :: Identifier -> Expr ann -> Expr ann -> Expr ann
swapOutVar matchIdent new =
  go
  where
    go (EVar _ ident) | matchIdent == ident = new
    go other = mapExpr go other

-- we end up with lots of `{ varName: varName }.varName , this turns them back
-- into `varName`
reduceRecords :: Identifier -> Expr ann -> Expr ann
reduceRecords ident =
  go
  where
    go expr@(ERecordAccess ann (ERecord _ items) accessIdent)
      | M.size items == 1 && ident == accessIdent =
          case M.lookup ident items of
            Just (EVar _ var) | var == ident -> EVar ann var
            _ -> expr
    go other = mapExpr go other

fromExpr :: Expr ann -> IExpr ann
fromExpr (ELambda ann ident body) =
  ILambda ann ident (\iExpr -> fromExpr (swapOutVar ident (toExpr iExpr) body))
fromExpr (EVar ann a) = IVar ann a
fromExpr (EPrim ann p) = IPrim ann p
fromExpr (EInfix ann op a b) = IInfix ann op (fromExpr a) (fromExpr b)
fromExpr (EApp ann fn arg) = IApp ann (fromExpr fn) (fromExpr arg)
fromExpr (EIf ann predE thenE elseE) =
  IIf ann (fromExpr predE) (fromExpr thenE) (fromExpr elseE)
fromExpr (ELet ann ident expr rest) =
  fromExpr (EApp ann (ELambda ann ident rest) expr)
fromExpr (EGlobalLet ann ident expr rest) =
  IGlobalLet ann ident (fromExpr expr) (fromExpr rest)
fromExpr (EGlobal ann ident) = IGlobal ann ident
fromExpr (EConstructor ann constructor) = IConstructor ann constructor
fromExpr (EPatternMatch ann patExpr pats) =
  let fromPat (pat, pExpr) =
        let patFnExpr iInput =
              let eInput = toExpr iInput
                  vars = patternVars pat
               in fromExpr
                    ( foldr
                        ( \ident totalExpr ->
                            swapOutVar ident (ERecordAccess ann eInput ident) totalExpr
                        )
                        pExpr
                        vars
                    )
         in (pat, patFnExpr)
   in IPatternMatch ann (fromExpr patExpr) (fromPat <$> pats)
fromExpr (ERecord ann as) = IRecord ann (fromExpr <$> as)
fromExpr (ERecordAccess ann a ident) = IRecordAccess ann (fromExpr a) ident
fromExpr (EAnn ann mt expr) = IAnn ann mt (fromExpr expr)
fromExpr (ETuple ann a as) = ITuple ann (fromExpr a) (fromExpr <$> as)

toExpr :: IExpr ann -> Expr ann
toExpr (ILambda ann ident fn) =
  ELambda ann ident (toExpr $ fn (IVar ann ident))
toExpr (IVar ann a) = EVar ann a
toExpr (IPrim ann p) = EPrim ann p
toExpr (IInfix ann op a b) = EInfix ann op (toExpr a) (toExpr b)
toExpr (IApp ann (ILambda _ ident rest) expr) =
  ELet ann ident (toExpr expr) (toExpr $ rest (IVar ann ident))
toExpr (IApp ann fn arg) = EApp ann (toExpr fn) (toExpr arg)
toExpr (IGlobalLet ann ident expr rest) =
  EGlobalLet ann ident (toExpr expr) (toExpr rest)
toExpr (IGlobal ann ident) = EGlobal ann ident
toExpr (IIf ann predE thenE elseE) =
  EIf ann (toExpr predE) (toExpr thenE) (toExpr elseE)
toExpr (IConstructor ann constructor) =
  EConstructor ann constructor
toExpr (IPatternMatch ann patExpr pats) =
  let toPat (pat, pExpr) =
        let vars = patternVars pat
            input = foldMap (\k -> M.singleton k (IVar ann k)) (S.toList vars)
            runPattern = toExpr (pExpr (IRecord ann input))
         in (pat, foldr reduceRecords runPattern vars)
   in EPatternMatch ann (toExpr patExpr) (toPat <$> pats)
toExpr (IRecord ann as) = ERecord ann (toExpr <$> as)
toExpr (IRecordAccess ann a ident) = ERecordAccess ann (toExpr a) ident
toExpr (IAnn ann mt expr) = EAnn ann mt (toExpr expr)
toExpr (ITuple ann a as) = ETuple ann (toExpr a) (toExpr <$> as)

patternVars :: Pattern ann -> Set Identifier
patternVars (PVar _ v) = S.singleton v
patternVars other = patternMonoid patternVars other