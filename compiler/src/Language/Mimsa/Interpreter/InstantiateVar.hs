module Language.Mimsa.Interpreter.InstantiateVar (instantiateVar, freshenVariable) where

import Language.Mimsa.ExprUtils
import Language.Mimsa.Interpreter.SwapName
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

identBinder :: Identifier var ann -> var
identBinder (Identifier _ var) = var
identBinder (AnnotatedIdentifier _ var) = var

addBinderToIdent :: Identifier var ann -> var -> Identifier var ann
addBinderToIdent (Identifier ann _) var =
  Identifier ann var
addBinderToIdent (AnnotatedIdentifier ann _) var =
  AnnotatedIdentifier ann var

-- | set all lambdas to fresh nwe variables
-- | TODO: optimise this with SwapName to do all these in one recursion, this
-- isn't super efficient right now
instantiateVar :: Expr Variable ann -> App ann (Expr Variable ann)
instantiateVar (MyLambda ann ident expr') = do
  (freshBinder, freshExpr) <- freshenVariable (identBinder ident) expr'
  subExpr <- instantiateVar freshExpr
  pure (MyLambda ann (addBinderToIdent ident freshBinder) subExpr)
instantiateVar other = bindExpr instantiateVar other

-- get new var, swapping out all lambdas
freshenVariable ::
  Variable ->
  Expr Variable ann ->
  App ann (Variable, Expr Variable ann)
freshenVariable name expr = do
  newName' <- nextVariable
  let newExpr = swapName name newName' expr
  copySwap name newName'
  pure (newName', newExpr)
