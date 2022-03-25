module Language.Mimsa.Actions.Interpret (interpreter) where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Interpreter2.Interpret
import Language.Mimsa.Interpreter2.MarkImports
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

-- get all the deps
-- change var to InterpretVar to point at imports
-- also collect Map ExprHash (Expr InterpretVar ann)
-- then interpret it
-- ...
-- profit?
interpreter :: StoreExpression Annotation -> Actions.ActionM (Expr Name Annotation)
interpreter se = do
  -- get dependencies of StoreExpression
  depsSe <- Actions.getDepsForStoreExpression se

  -- optimise them all like a big legend
  let depsMap = markAllImports (fst <$> depsSe)

  rootExpr <- case M.lookup (getStoreExpressionHash se) depsMap of
    Just re -> pure re
    _ -> throwError (StoreErr (CouldNotFindStoreExpression (getStoreExpressionHash se)))

  interpretedExpr <- liftEither (first InterpreterErr2 (interpret depsMap rootExpr))

  let exprWithoutExprHashes = first fst interpretedExpr

  pure (exprWithoutExprHashes $> mempty)

-- Turns a pile of store expressions into a map of Exprs ready to interpret
-- this means their variables are marked as either Local or Import (pointing at
-- where they come from)
markAllImports ::
  Map ExprHash (StoreExpression Annotation) ->
  Map ExprHash (Expr (Name, Maybe ExprHash) Annotation)
markAllImports inputStoreExpressions =
  convertImports <$> inputStoreExpressions
