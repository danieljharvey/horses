{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Substitutor
  ( mapVar,
    substitute,
  )
where

import Control.Monad (join)
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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

type App = State (Swaps, Scope)

substitute :: Store -> StoreExpression -> (Swaps, Expr, Scope)
substitute store' storeExpr =
  let (expr', (swaps', scope')) = runSubApp store' storeExpr
   in (swaps', expr', scope')

runSubApp :: Store -> StoreExpression -> (Expr, (Swaps, Scope))
runSubApp store' storeExpr =
  runState (doSubstitutions store' storeExpr) (mempty, mempty)

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

findInScope :: Name -> App (Maybe Expr)
findInScope name = do
  (swaps', Scope scope') <- get
  case filter (\(_, v) -> v == name) (M.toList swaps') of
    [] -> pure Nothing
    ((k, _) : _) -> do
      pure $ M.lookup k scope'

-- get a new name for a var, changing it's reference in Scope and adding it to
-- Swaps list
-- we don't do this for built-ins (ie, randomInt) or variables introduced by
-- lambdas
getNextVar :: [Name] -> Name -> App Name
getNextVar protected name =
  if elem name protected || isLibraryName (name)
    then pure name
    else do
      let makeName :: Int -> Name
          makeName i = mkName $ "var" <> T.pack (show i)
      nextName <- makeName <$> gets (M.size . fst)
      (swaps', (Scope scope')) <- get
      found <- findInScope name
      let newScope = case found of
            (Just expr') ->
              Scope $ M.insert nextName expr' scope'
            Nothing ->
              Scope $
                M.mapKeys
                  ( \key ->
                      if key == name
                        then nextName
                        else key
                  )
                  scope'
      let newSwaps = M.insert nextName name swaps'
      put (newSwaps, newScope)
      pure nextName

-- step through Expr, replacing vars with numbered variables
mapVar :: [Name] -> Expr -> App Expr
mapVar p (MyVar a) =
  MyVar <$> getNextVar p a
mapVar p (MyLet name a b) =
  MyLet <$> pure name <*> (mapVar p a)
    <*> (mapVar (p <> [name]) b)
mapVar p (MyLambda name a) =
  MyLambda <$> pure name <*> (mapVar (p <> [name]) a)
mapVar p (MyRecordAccess a name) = MyRecordAccess <$> (mapVar p a) <*> pure name
mapVar p (MyApp a b) = MyApp <$> (mapVar p a) <*> (mapVar p b)
mapVar p (MyIf a b c) = MyIf <$> (mapVar p a) <*> (mapVar p b) <*> (mapVar p c)
mapVar p (MyPair a b) = MyPair <$> (mapVar p a) <*> (mapVar p b)
mapVar p (MyLetPair nameA nameB a b) =
  MyLetPair
    <$> pure nameA <*> pure nameB
      <*> (mapVar (p <> [nameA, nameB]) a)
      <*> (mapVar (p <> [nameA, nameB]) b)
mapVar p (MyLetList nameHead nameRest a b) =
  MyLetList <$> pure nameHead
    <*> pure nameRest
    <*> (mapVar (p <> [nameHead, nameRest]) a)
    <*> (mapVar (p <> [nameHead, nameRest]) b)
mapVar p (MySum side a) = MySum <$> pure side <*> (mapVar p a)
mapVar p (MyCase a b c) =
  MyCase <$> (mapVar p a) <*> (mapVar p b)
    <*> (mapVar p c)
mapVar p (MyList as) = do
  mas <- traverse (mapVar p) as
  pure (MyList mas)
mapVar p (MyRecord map') = do
  map2 <- traverse (mapVar p) map'
  pure (MyRecord map2)
mapVar _ (MyLiteral a) = pure (MyLiteral a)
