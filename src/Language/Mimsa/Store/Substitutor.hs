{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Substitutor
  ( mapVar,
    substitute,
  )
where

import Control.Monad (join)
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor (first, second)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Library (isLibraryName)
import Language.Mimsa.Types

-- this turns StoreExpressions back into expressions by substituting their
-- variables for the deps passed in
--
-- like the typechecker, as we go though, we replace the varibable names with
-- var0, var1, var2 etc so we don't have to care about scoping or collisions
--
-- we'll also store what our substitutions were for errors sake

type Swaps = Map Name Name

type App = StateT (Swaps, Scope) (Either Text)

substitute :: Store -> StoreExpression -> Either Text (Swaps, Expr, Scope)
substitute store' storeExpr = do
  (expr', (swaps', scope')) <- runSubApp store' storeExpr
  pure (swaps', expr', scope')

runSubApp :: Store -> StoreExpression -> Either Text (Expr, (Swaps, Scope))
runSubApp store' storeExpr =
  runStateT (doSubstitutions store' storeExpr) (mempty, mempty)

-- get the list of deps for this expression, turn from hashes to StoreExprs
doSubstitutions :: Store -> StoreExpression -> App Expr
doSubstitutions store' (StoreExpression bindings' expr) = do
  let scope = createScope store' bindings'
  modify (second $ (<>) scope)
  newExpr <- mapVar [] expr
  _ <-
    traverse
      ( \(key, storeExpr') -> do
          expr' <- doSubstitutions store' storeExpr'
          newKey <- substituteKey key <$> gets (fst)
          modify (second $ (<>) (Scope (M.singleton newKey expr')))
          pure ()
      )
      (getExprPairs store' bindings')
  pure newExpr

-- give me the original key, i'll give you the new one
substituteKey :: Name -> Swaps -> Name
substituteKey name swaps = fromMaybe (Name "notfound") found
  where
    matches = M.toList $ M.filter (\v -> v == name) swaps
    found = case matches of
      [] -> Nothing
      ((k, _) : _) -> Just k

-- get everything our nice function wants and plop it in a pile
createScope :: Store -> Bindings -> Scope
createScope store' bindings' = Scope . M.fromList $ pairs'
  where
    pairs = getExprPairs store' bindings'
    pairs' = (\(a, (StoreExpression _ b)) -> (a, b)) <$> pairs

getExprPairs :: Store -> Bindings -> [(Name, StoreExpression)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

-- get a new name for a var, changing it's reference in Scope and adding it to
-- Swaps list
getNextVar :: [Name] -> Name -> App Name
getNextVar protected name =
  if elem name protected || isLibraryName name
    then pure name
    else do
      let makeName :: Int -> Name
          makeName i = mkName $ "var" <> T.pack (show i)
      nextName <- makeName <$> fst <$> gets (first $ M.size)
      modify (second $ \(Scope scope') -> Scope $ M.mapKeys (\key -> if key == name then nextName else key) scope')
      modify (first $ M.insert nextName name)
      pure nextName

-- step through Expr, replacing vars with numbered variables
mapVar :: [Name] -> Expr -> App Expr
mapVar p (MyVar a) = MyVar <$> getNextVar p a
mapVar p (MyLet name a b) = MyLet <$> pure name <*> (mapVar p a) <*> (mapVar p b)
mapVar p (MyLambda name a) =
  MyLambda <$> pure name <*> (mapVar (p <> [name]) a)
mapVar p (MyApp a b) = MyApp <$> (mapVar p a) <*> (mapVar p b)
mapVar p (MyIf a b c) = MyIf <$> (mapVar p a) <*> (mapVar p b) <*> (mapVar p c)
mapVar _ a = pure a
