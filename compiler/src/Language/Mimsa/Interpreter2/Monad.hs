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

addStackFrame :: StackFrame var ann -> Stack var ann -> Stack var ann
addStackFrame sf (Stack stack) = Stack (sf <| stack)

mapTopFrame :: (StackFrame var ann -> StackFrame var ann) -> Stack var ann -> Stack var ann
mapTopFrame f (Stack stack) =
  let currentFrame = NE.head stack
      restOfStack = NE.tail stack
   in Stack (f currentFrame :| restOfStack)

addVarToFrame :: (Ord var) => var -> Expr var (StackFrame var ann) -> StackFrame var ann -> StackFrame var ann
addVarToFrame var expr (StackFrame entries infixes) =
  StackFrame (M.singleton var expr <> entries) infixes

addToStackFrame :: (Ord var) => var -> Expr var (StackFrame var ann) -> Stack var ann -> Stack var ann
addToStackFrame var expr =
  mapTopFrame (addVarToFrame var expr)

getCurrentStackFrame :: InterpreterM var ann (StackFrame var ann)
getCurrentStackFrame = asks (NE.head . getStack)

lookupVar :: (Ord var) => var -> InterpreterM var ann (Expr var (StackFrame var ann))
lookupVar var = do
  (StackFrame entries _) <- getCurrentStackFrame
  case M.lookup var entries of
    Just entry -> pure entry
    _ -> throwError (CouldNotFindVar entries var)

addOperator :: InfixOp -> InterpretExpr var ann -> Stack var ann -> Stack var ann
addOperator infixOp expr = do
  mapTopFrame (addOperatorToFrame infixOp expr)

addOperatorToFrame :: InfixOp -> InterpretExpr var ann -> StackFrame var ann -> StackFrame var ann
addOperatorToFrame infixOp expr (StackFrame entries infixes) =
  StackFrame entries (M.singleton infixOp expr <> infixes)

findOperator :: InfixOp -> InterpreterM var ann (InterpretExpr var ann)
findOperator infixOp = do
  (StackFrame _ infixes) <- getCurrentStackFrame
  case M.lookup infixOp infixes of
    Just entry -> pure entry
    _ -> error "could not find op" -- throwError (CouldNotFindVar infixes infixOp)
