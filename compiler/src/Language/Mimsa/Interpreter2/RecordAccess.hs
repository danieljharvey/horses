module Language.Mimsa.Interpreter2.RecordAccess (interpretRecordAccess) where

import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Interpreter.Stack

interpretRecordAccess ::
  InterpretFn var ann ->
  ExprData var ann ->
  InterpretExpr var ann ->
  Name ->
  InterpreterM var ann (InterpretExpr var ann)
interpretRecordAccess interpretFn _ (MyRecord _ record) name =
  case M.lookup name record of
    Just item -> interpretFn item
    _ -> throwError $ CannotFindMemberInRecord record name
interpretRecordAccess interpretFn ann (MyVar ann' a) name = do
  intExpr <- interpretFn (MyVar ann' a)
  interpretFn (MyRecordAccess ann intExpr name)
interpretRecordAccess interpretFn ann (MyRecordAccess ann' a name') name = do
  intExpr <- interpretFn (MyRecordAccess ann' a name')
  interpretFn (MyRecordAccess ann intExpr name)
interpretRecordAccess _ _ recordExpr name =
  throwError $ CannotDestructureAsRecord recordExpr name
