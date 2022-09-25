module Language.Mimsa.Interpreter.RecordAccess (interpretRecordAccess, interpretTupleAccess) where

import Language.Mimsa.Interpreter.ToHOAS
import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import GHC.Natural
import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST.HOASExpr
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Interpreter.Stack

interpretRecordAccess ::
  InterpretFn var ann ->
  ExprData ann ->
  InterpretExpr var ann ->
  Name ->
  InterpreterM var ann (InterpretExpr var ann)
interpretRecordAccess interpretFn _ (MyRecord _ record) name =
  case M.lookup name record of
    Just item -> interpretFn item
    _ -> throwError $ CannotFindMemberInRecord (fromHOAS <$> record) name
interpretRecordAccess interpretFn ann (MyVar ann' modName a) name = do
  intExpr <- interpretFn (MyVar ann' modName a)
  interpretFn (MyRecordAccess ann intExpr name)
interpretRecordAccess interpretFn ann (MyRecordAccess ann' a name') name = do
  intExpr <- interpretFn (MyRecordAccess ann' a name')
  interpretFn (MyRecordAccess ann intExpr name)
interpretRecordAccess interpretFn ann (MyApp ann' fn arg) name = do
  res <- interpretFn (MyApp ann' fn arg)
  interpretFn (MyRecordAccess ann res name)
interpretRecordAccess _ _ recordExpr name =
  throwError $ CannotDestructureAsRecord (fromHOAS recordExpr) name

interpretTupleAccess ::
  InterpretFn var ann ->
  ExprData var ann ->
  InterpretExpr var ann ->
  Natural ->
  InterpreterM var ann (InterpretExpr var ann)
interpretTupleAccess interpretFn _ (MyTuple _ a as) index =
  let allItems = [a] <> NE.toList as
   in case listToMaybe (drop (fromIntegral index - 1) allItems) of
        Just item -> interpretFn item
        _ -> throwError $ CannotFindMemberInTuple allItems index
interpretTupleAccess interpretFn ann (MyVar ann' modName a) index = do
  intExpr <- interpretFn (MyVar ann' modName a)
  interpretFn (MyTupleAccess ann intExpr index)
interpretTupleAccess interpretFn ann (MyTupleAccess ann' a index') index = do
  intExpr <- interpretFn (MyTupleAccess ann' a index')
  interpretFn (MyTupleAccess ann intExpr index)
interpretTupleAccess _ _ recordExpr index =
  throwError $ CannotDestructureAsTuple recordExpr index
