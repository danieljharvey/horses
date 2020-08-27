module Language.Mimsa.Store.Substitutor where

import Control.Monad (join)
import Control.Monad.Trans.State.Lazy
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Library (isLibraryName)
import Language.Mimsa.Types

-- this turns StoreExpressions back into expressions by substituting their
-- variables for the deps passed in
--
-- like the typechecker, as we go though, we replace the varibable names with
-- var0, var1, var2 etc so we don't have to care about scoping or collisions
--
-- we'll also store what our substitutions were for errors sake

data SubsState
  = SubsState
      { subsSwaps :: Swaps,
        subsScope :: Scope,
        subsCounter :: Int
      }

type App = State SubsState

substitute :: Store -> StoreExpression -> (Swaps, Expr Variable, Scope)
substitute store' storeExpr =
  let startingState = SubsState mempty mempty 0
      (expr', SubsState swaps' scope' _) =
        runState
          (doSubstitutions store' storeExpr)
          startingState
   in (swaps', expr', scope')

-- get the list of deps for this expression, turn from hashes to StoreExprs
doSubstitutions :: Store -> StoreExpression -> App (Expr Variable)
doSubstitutions store' (StoreExpression bindings' expr) = do
  newScopes <- traverse (substituteWithKey store') (getExprPairs store' bindings')
  addScope $ mconcat newScopes
  mapVar [] expr

substituteWithKey :: Store -> (Name, StoreExpression) -> App Scope
substituteWithKey store' (key, storeExpr') = do
  expr' <- doSubstitutions store' storeExpr'
  maybeKey <- scopeExists expr'
  key' <- case maybeKey of
    Just existingKey -> pure existingKey
    Nothing -> getNextVar [] key
  pure $ Scope (M.singleton key' expr')

-- if the expression we're already saving is in the scope
-- that's the name we want to use
scopeExists :: Expr Variable -> App (Maybe Variable)
scopeExists var = do
  (Scope scope') <- gets subsScope
  pure (mapKeyFind (var ==) scope')

addScope :: Scope -> App ()
addScope scope' =
  modify (\s -> s {subsScope = subsScope s <> scope'})

addSwap :: Name -> Variable -> App ()
addSwap old new =
  modify (\s -> s {subsSwaps = subsSwaps s <> M.singleton new old})

mapKeyFind :: (a -> Bool) -> Map k a -> Maybe k
mapKeyFind pred' map' = case M.toList (M.filter pred' map') of
  [] -> Nothing
  ((k, _) : _) -> pure k

-- if we've already made up a name for this var, return that
findInSwaps :: Name -> App (Maybe Variable)
findInSwaps name = do
  swaps' <- gets subsSwaps
  pure (mapKeyFind (name ==) swaps')

getExprPairs :: Store -> Bindings -> [(Name, StoreExpression)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

-- get a new name for a var, changing it's reference in Scope and adding it to
-- Swaps list
-- we don't do this for built-ins (ie, randomInt) or variables introduced by
-- lambdas
getNextVar :: [Name] -> Name -> App Variable
getNextVar protected name
  | name `elem` protected = pure (NamedVar name)
  | isLibraryName name = pure (BuiltIn name)
  | otherwise = do
    stuff <- findInSwaps name
    case stuff of
      Just existingName -> pure existingName
      Nothing -> do
        nextName <- NumberedVar <$> nextNum
        addSwap name nextName
        pure nextName

nextNum :: App Int
nextNum = do
  p <- gets subsCounter
  modify (\s -> s {subsCounter = p + 1})
  pure p

nameToVar :: (Monad m) => Name -> m Variable
nameToVar = pure . NamedVar

-- step through Expr, replacing vars with numbered variables
mapVar :: [Name] -> Expr Name -> App (Expr Variable)
mapVar p (MyVar a) =
  MyVar <$> getNextVar p a
mapVar p (MyLet name a b) =
  MyLet <$> nameToVar name <*> mapVar p a
    <*> mapVar (p <> [name]) b
mapVar p (MyLambda name a) =
  MyLambda <$> nameToVar name <*> mapVar (p <> [name]) a
mapVar p (MyRecordAccess a name) =
  MyRecordAccess
    <$> mapVar p a <*> pure name
mapVar p (MyApp a b) = MyApp <$> mapVar p a <*> mapVar p b
mapVar p (MyIf a b c) = MyIf <$> mapVar p a <*> mapVar p b <*> mapVar p c
mapVar p (MyPair a b) = MyPair <$> mapVar p a <*> mapVar p b
mapVar p (MyLetPair nameA nameB a b) =
  MyLetPair
    <$> nameToVar nameA <*> nameToVar nameB
      <*> mapVar (p <> [nameA, nameB]) a
      <*> mapVar (p <> [nameA, nameB]) b
mapVar p (MyRecord map') = do
  map2 <- traverse (mapVar p) map'
  pure (MyRecord map2)
mapVar _ (MyLiteral a) = pure (MyLiteral a)
mapVar p (MyData a b c d) = MyData a b c <$> mapVar p d
mapVar _ (MyConstructor name) = pure (MyConstructor name)
mapVar p (MyConsApp fn var) = MyConsApp <$> mapVar p fn <*> mapVar p var
mapVar p (MyCaseMatch expr' matches catchAll) = do
  let mapVarPair (name, expr'') = (,) <$> pure name <*> mapVar p expr''
  matches' <- traverse mapVarPair matches
  catchAll' <- traverse (mapVar p) catchAll
  MyCaseMatch <$> mapVar p expr' <*> pure matches' <*> pure catchAll'
