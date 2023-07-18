module Smol.Core.Interpreter.RecordAccess (interpretRecordAccess ) where

import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import GHC.Natural
import Smol.Core.Interpreter.Types
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Types.Identifier
import Smol.Core.Types.Expr

interpretRecordAccess ::
  InterpretFn ann ->
  ExprData ann ->
  InterpretExpr ann ->
  Identifier ->
  InterpreterM ann (InterpretExpr ann)
interpretRecordAccess interpretFn _ (ERecord _ record) name =
  case M.lookup name record of
    Just item -> interpretFn item
    _ -> throwError $ CannotFindMemberInRecord record name
interpretRecordAccess interpretFn ann (EVar ann' a) name = do
  intExpr <- interpretFn (EVar ann' a)
  interpretFn (ERecordAccess ann intExpr name)
interpretRecordAccess interpretFn ann (ERecordAccess ann' a name') name = do
  intExpr <- interpretFn (ERecordAccess ann' a name')
  interpretFn (ERecordAccess ann intExpr name)
interpretRecordAccess interpretFn ann (EApp ann' fn arg) name = do
  res <- interpretFn (EApp ann' fn arg)
  interpretFn (ERecordAccess ann res name)
interpretRecordAccess _ _ recordExpr name = do
  throwError $ CannotDestructureAsRecord recordExpr name
