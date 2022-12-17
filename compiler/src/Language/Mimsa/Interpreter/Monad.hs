module Language.Mimsa.Interpreter.Monad
  ( findOperator,
    lookupVar,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Language.Mimsa.Core
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker.Unique

lookupVar ::
  (Name, Unique) ->
  InterpreterM ann (Maybe (InterpretExpr ann))
lookupVar (_, Dependency exprHash) = Just <$> lookupInGlobals exprHash
lookupVar _ = pure Nothing

-- lookupVar  other  = error $ "looking up " <> show other

lookupInGlobals :: ExprHash -> InterpreterM ann (InterpretExpr ann)
lookupInGlobals exprHash = do
  globals <- asks ireGlobals
  case M.lookup exprHash globals of
    Just found -> pure found
    _ -> throwError (CouldNotFindGlobal (fromHOAS <$> globals) exprHash)

-- lookup custom infixOp in global scope
findOperator :: InfixOp -> InterpreterM ann (InterpretExpr ann)
findOperator infixOp = do
  allInfixes <- asks ireInfixes
  case M.lookup infixOp allInfixes of
    Just infixHash -> lookupInGlobals infixHash
    _ -> throwError (CouldNotFindInfix infixOp)
