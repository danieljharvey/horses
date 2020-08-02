module Language.Mimsa.Project.Versions (findVersions) where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import Language.Mimsa.Actions
import Language.Mimsa.Project.Usages
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.StoreExpression
import Language.Mimsa.Types.Usage
import Language.Mimsa.Types.Variable

-- find which versions of a given binding are in use

findVersions ::
  Project ->
  Name ->
  Either Error (NonEmpty (Int, Expr Variable, MonoType, Set Usage))
findVersions project name = do
  versioned <- first UsageErr (findInProject project name)
  as <- traverse (getExprDetails project) versioned
  let nice = NE.zip (NE.fromList [1 ..]) as
  pure $ NE.reverse $ (\(i, (a, b, c)) -> (i, a, b, c)) <$> nice

findInProject :: Project -> Name -> Either UsageError (NonEmpty ExprHash)
findInProject project name =
  case M.lookup name (getVersionedBindings $ bindings project) of
    Just versioned -> Right versioned
    _ -> throwError $ CouldNotFindBinding name

getStoreExpression ::
  Project ->
  ExprHash ->
  Either UsageError StoreExpression
getStoreExpression (Project store' _) exprHash =
  case M.lookup exprHash (getStore store') of
    Just storeExpression' -> Right storeExpression'
    _ -> Left (CouldNotFindStoreExpression exprHash)

getExprDetails ::
  Project ->
  ExprHash ->
  Either Error (Expr Variable, MonoType, Set Usage)
getExprDetails project exprHash = do
  usages <- first UsageErr (findUsages project exprHash)
  storeExpr <- first UsageErr (getStoreExpression project exprHash)
  (mt, _, expr', _) <- evaluateStoreExpression (store project) storeExpr
  pure (expr', mt, usages)
