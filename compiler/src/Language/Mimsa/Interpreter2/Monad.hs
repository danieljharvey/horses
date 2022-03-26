module Language.Mimsa.Interpreter2.Monad where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash

addStackFrame :: StackFrame var ann -> InterpreterM var ann a -> InterpreterM var ann a
addStackFrame sf fn = do
  let addStack (Stack stack) = Stack (sf <| stack)
  local
    (\ire -> ire {ireStack = addStack (ireStack ire)})
    fn

mapTopFrame :: (StackFrame var ann -> StackFrame var ann) -> Stack var ann -> Stack var ann
mapTopFrame f (Stack stack) =
  let currentFrame = NE.head stack
      restOfStack = NE.tail stack
   in Stack (f currentFrame :| restOfStack)

addVarToFrame ::
  (Ord var) =>
  (var, Maybe ExprHash) ->
  InterpretExpr var ann ->
  StackFrame var ann ->
  StackFrame var ann
addVarToFrame (var, _) expr (StackFrame entries infixes) =
  StackFrame (M.singleton var expr <> entries) infixes

addToStackFrame :: (Ord var) => (var, Maybe ExprHash) -> InterpretExpr var ann -> InterpreterM var ann a -> InterpreterM var ann a
addToStackFrame var expr =
  local
    (\ire -> ire {ireStack = mapTopFrame (addVarToFrame var expr) (ireStack ire)})

getCurrentStackFrame :: InterpreterM var ann (StackFrame var ann)
getCurrentStackFrame = asks (NE.head . getStack . ireStack)

lookupInGlobals :: ExprHash -> InterpreterM var ann (InterpretExpr var ann)
lookupInGlobals exprHash = do
  globals <- asks ireGlobals
  case M.lookup exprHash globals of
    Just found -> pure found
    _ -> throwError (CouldNotFindGlobal globals exprHash)

lookupVar ::
  (Ord var) =>
  (var, Maybe ExprHash) ->
  InterpreterM var ann (InterpretExpr var ann)
lookupVar (var, maybeExprHash) =
  case maybeExprHash of
    Just exprHash -> lookupInGlobals exprHash
    Nothing -> do
      (StackFrame entries _) <- getCurrentStackFrame
      case M.lookup var entries of
        Just lam@MyLambda {} ->
          -- when we save functions on the stack we save them as
          -- \letName -> function
          -- so that recursion works
          -- therefore when fetching it we apply it to itself
          -- like a fixpoint combinator thing
          pure (MyApp mempty lam lam)
        Just other -> pure other
        _ -> throwError (CouldNotFindVar entries var)

addOperator :: InfixOp -> InterpretExpr var ann -> InterpreterM var ann a -> InterpreterM var ann a
addOperator infixOp expr = do
  local
    ( \ire ->
        ire
          { ireStack =
              mapTopFrame (addOperatorToFrame infixOp expr) (ireStack ire)
          }
    )

addOperatorToFrame :: InfixOp -> InterpretExpr var ann -> StackFrame var ann -> StackFrame var ann
addOperatorToFrame infixOp expr (StackFrame entries infixes) =
  StackFrame entries (M.singleton infixOp expr <> infixes)

findOperator :: InfixOp -> InterpreterM var ann (InterpretExpr var ann)
findOperator infixOp = do
  (StackFrame _ infixes) <- getCurrentStackFrame
  case M.lookup infixOp infixes of
    Just entry -> pure entry
    _ -> error "could not find op" -- throwError (CouldNotFindVar infixes infixOp)
