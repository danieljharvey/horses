module Smol.Interpreter.Monad
  ( withNewStackFrame,
    extendStackFrame,
    getCurrentStackFrame,
    lookupVar,
    addVarToFrame,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.ResolvedDep
import Smol.Interpreter.Types
import Smol.Interpreter.Types.InterpreterError
import Smol.Interpreter.Types.Stack

-- | run action with entirely new frame
-- | useful for running functions from their closures
withNewStackFrame ::
  StackFrame ann ->
  InterpreterM ann a ->
  InterpreterM ann a
withNewStackFrame sf =
  local
    (\ire -> ire {ireStack = sf})

extendStackFrame ::
  [ ( ResolvedDep Identifier,
      InterpretExpr ann
    )
  ] ->
  InterpreterM ann a ->
  InterpreterM ann a
extendStackFrame bindings =
  local
    ( \ire ->
        ire
          { ireStack =
              foldr (uncurry addVarToFrame) (ireStack ire) bindings
          }
    )

getCurrentStackFrame :: InterpreterM ann (StackFrame ann)
getCurrentStackFrame = asks ireStack

lookupVar ::
  (Show ann) =>
  ResolvedDep Identifier ->
  InterpreterM ann (InterpretExpr ann)
lookupVar identifier =
  lookupVarInStack identifier
    `catchError` \_ -> lookupVarInDeps identifier

lookupVarInDeps ::
  ResolvedDep Identifier ->
  InterpreterM ann (InterpretExpr ann)
lookupVarInDeps identifier = do
  matchingDep <- asks (M.lookup identifier . ireGlobals)
  case matchingDep of
    Just a -> pure a
    Nothing -> throwError (CouldNotFindVar mempty identifier)

lookupVarInStack ::
  (Show ann) =>
  ResolvedDep Identifier ->
  InterpreterM ann (InterpretExpr ann)
lookupVarInStack identifier = do
  (StackFrame entries) <- getCurrentStackFrame
  case M.lookup identifier entries of
    Just myLam@(ELambda ed@(ExprData _ isRec _) _ _) ->
      -- when we save functions on the stack we save them as
      -- \letName -> function
      -- so that recursion works
      -- therefore when fetching it we apply it to itself
      -- like a fixpoint combinator thing
      if isRec
        then pure (EApp ed myLam myLam)
        else pure myLam
    -- if it's another var, fetch that
    Just (EVar _ a) -> lookupVar a
    -- otherwise return it
    Just other -> pure other
    -- could not find var
    _ -> throwError (CouldNotFindVar entries identifier)

addVarToFrame ::
  ResolvedDep Identifier ->
  InterpretExpr ann ->
  StackFrame ann ->
  StackFrame ann
addVarToFrame identifier expr (StackFrame entries) =
  StackFrame (M.singleton identifier expr <> entries)
