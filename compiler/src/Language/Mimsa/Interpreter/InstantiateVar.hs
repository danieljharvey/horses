module Language.Mimsa.Interpreter.InstantiateVar (instantiateVar) where

import Language.Mimsa.ExprUtils
import Language.Mimsa.Interpreter.SwapName
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- | set all lambdas to fresh nwe variables
-- | TODO: optimise this with SwapName to do all these in one recursion, this
-- isn't super efficient right now
instantiateVar :: Expr Variable ann -> App ann (Expr Variable ann)
instantiateVar (MyLambda ann (Identifier bindAnn binder) expr') = do
  (freshBinder, freshExpr) <- newLambdaCopy binder expr'
  subExpr <- instantiateVar freshExpr
  pure (MyLambda ann (Identifier bindAnn freshBinder) subExpr)
instantiateVar other = bindExpr instantiateVar other

-- get new var, swapping out all lambdas
newLambdaCopy ::
  Variable ->
  Expr Variable ann ->
  App ann (Variable, Expr Variable ann)
newLambdaCopy name expr = do
  newName' <- nextVariable
  newExpr <- swapName name newName' expr
  copySwap name newName'
  pure (newName', newExpr)
