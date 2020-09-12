module Language.Mimsa.Project.Versions (findVersions, findStoreExpressionByName) where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import Language.Mimsa.Actions
import Language.Mimsa.Project.Usages
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.StoreExpression
import Language.Mimsa.Types.Usage
import Language.Mimsa.Types.VersionedMap

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
  case M.lookup name (getVersionedMap $ bindings project) of
    Just versioned -> Right versioned
    _ -> throwError $ CouldNotFindBinding name

getStoreExpression ::
  Project ->
  ExprHash ->
  Either UsageError StoreExpression
getStoreExpression (Project store' _ _ _) exprHash =
  case M.lookup exprHash (getStore store') of
    Just storeExpression' -> Right storeExpression'
    _ -> Left (CouldNotFindStoreExpression exprHash)

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

findStoreExpressionByName :: Project -> Name -> Maybe StoreExpression
findStoreExpressionByName env name =
  case findInProject env name of
    Right hashes -> hush $ getStoreExpression env (NE.last hashes)
    _ -> Nothing

getExprDetails ::
  Project ->
  ExprHash ->
  Either Error (Expr Variable, MonoType, Set Usage)
getExprDetails project exprHash = do
  usages <- first UsageErr (findUsages project exprHash)
  storeExpr <- first UsageErr (getStoreExpression project exprHash)
  (mt, _, expr', _, _) <- evaluateStoreExpression (store project) storeExpr
  pure (expr', mt, usages)
