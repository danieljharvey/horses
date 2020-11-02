module Language.Mimsa.Store.Substitutor where

import Control.Monad (join)
import Control.Monad.Trans.State.Lazy
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.SubstitutedExpression
import Language.Mimsa.Types.Swaps

-- this turns StoreExpressions back into expressions by substituting their
-- variables for the deps passed in
--
-- like the typechecker, as we go though, we replace the varibable names with
-- var0, var1, var2 etc so we don't have to care about scoping or collisions
--
-- we'll also store what our substitutions were for errors sake

data SubsState ann
  = SubsState
      { subsSwaps :: Swaps,
        subsScope :: Scope ann,
        subsCounter :: Int
      }

type App ann = State (SubsState ann)

substitute ::
  (Eq ann, Monoid ann) =>
  Store ann ->
  StoreExpression ann ->
  SubstitutedExpression ann
substitute store' storeExpr =
  let startingState = SubsState mempty mempty 0
      (expr', SubsState swaps' scope' _) =
        runState
          (doSubstitutions store' storeExpr)
          startingState
   in SubstitutedExpression
        swaps'
        expr'
        scope'

-- get the list of deps for this expression, turn from hashes to StoreExprs
doSubstitutions ::
  (Monoid ann, Eq ann) =>
  Store ann ->
  StoreExpression ann ->
  App ann (Expr Variable ann)
doSubstitutions store' (StoreExpression expr bindings' tBindings) = do
  newScopes <- traverse (substituteWithKey store') (getExprPairs store' bindings')
  addScope $ mconcat newScopes
  expr' <- mapVar [] expr
  dtList <- toDataTypeList <$> resolveTypes store' tBindings
  pure $ addDataTypesToExpr expr' dtList

------------ type stuff

-- why doesn't this exist already
lookupStoreItem :: Store ann -> ExprHash -> Maybe (StoreExpression ann)
lookupStoreItem (Store s') hash' = M.lookup hash' s'

-- find all types needed by our expression in the store
resolveTypes :: Store ann -> TypeBindings -> App ann [StoreExpression ann]
resolveTypes store' (TypeBindings tBindings) =
  let typeLookup (_, hash) = case lookupStoreItem store' hash of
        Just sExpr -> pure [sExpr]
        _ -> pure mempty
   in mconcat <$> traverse typeLookup (M.toList tBindings)

toDataTypeList :: [StoreExpression ann] -> [DataType]
toDataTypeList sExprs =
  let getDts sExpr = extractDataTypes (storeExpression sExpr)
   in S.toList . mconcat $ (getDts <$> sExprs)

-- add required type declarations into our Expr
addDataTypesToExpr :: (Monoid ann) => Expr var ann -> [DataType] -> Expr var ann
addDataTypesToExpr =
  foldr (MyData mempty)

--------------

substituteWithKey ::
  (Eq ann, Monoid ann) =>
  Store ann ->
  (Name, StoreExpression ann) ->
  App ann (Scope ann)
substituteWithKey store' (key, storeExpr') = do
  expr' <- doSubstitutions store' storeExpr'
  maybeKey <- scopeExists expr'
  key' <- case maybeKey of
    Just existingKey -> pure existingKey
    Nothing -> getNextVar [] key
  pure $ Scope (M.singleton key' expr')

-- if the expression we're already saving is in the scope
-- that's the name we want to use
scopeExists :: (Eq ann) => Expr Variable ann -> App ann (Maybe Variable)
scopeExists var = do
  (Scope scope') <- gets subsScope
  pure (mapKeyFind (var ==) scope')

addScope :: Scope ann -> App ann ()
addScope scope' =
  modify (\s -> s {subsScope = subsScope s <> scope'})

addSwap :: Name -> Variable -> App ann ()
addSwap old new =
  modify (\s -> s {subsSwaps = subsSwaps s <> M.singleton new old})

mapKeyFind :: (a -> Bool) -> Map k a -> Maybe k
mapKeyFind pred' map' = case M.toList (M.filter pred' map') of
  [] -> Nothing
  ((k, _) : _) -> pure k

-- if we've already made up a name for this var, return that
findInSwaps :: Name -> App ann (Maybe Variable)
findInSwaps name = do
  swaps' <- gets subsSwaps
  pure (mapKeyFind (name ==) swaps')

getExprPairs :: Store ann -> Bindings -> [(Name, StoreExpression ann)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

-- get a new name for a var, changing it's reference in Scope and adding it to
-- Swaps list
-- we don't do this for variables introduced by
-- lambdas
getNextVar ::
  (Eq ann, Monoid ann) =>
  [Name] ->
  Name ->
  App ann Variable
getNextVar protected name
  | name `elem` protected = pure (NamedVar name)
  | otherwise = do
    stuff <- findInSwaps name
    case stuff of
      Just existingName -> pure existingName
      Nothing -> do
        nextName <- NumberedVar <$> nextNum
        addSwap name nextName
        pure nextName

nextNum :: App ann Int
nextNum = do
  p <- gets subsCounter
  modify (\s -> s {subsCounter = p + 1})
  pure p

nameToVar :: (Monad m) => Name -> m Variable
nameToVar = pure . NamedVar

-- step through Expr, replacing vars with numbered variables
mapVar ::
  (Eq ann, Monoid ann) =>
  [Name] ->
  Expr Name ann ->
  App ann (Expr Variable ann)
mapVar p (MyVar ann a) =
  MyVar ann <$> getNextVar p a
mapVar p (MyLet ann name a b) =
  MyLet ann <$> nameToVar name <*> mapVar p a
    <*> mapVar (p <> [name]) b
mapVar p (MyInfix ann op a b) =
  MyInfix ann op <$> mapVar p a <*> mapVar p b
mapVar p (MyLambda ann name a) =
  MyLambda ann <$> nameToVar name <*> mapVar (p <> [name]) a
mapVar p (MyRecordAccess ann a name) =
  MyRecordAccess ann
    <$> mapVar p a <*> pure name
mapVar p (MyApp ann a b) = MyApp ann <$> mapVar p a <*> mapVar p b
mapVar p (MyIf ann a b c) = MyIf ann <$> mapVar p a <*> mapVar p b <*> mapVar p c
mapVar p (MyPair ann a b) = MyPair ann <$> mapVar p a <*> mapVar p b
mapVar p (MyLetPair ann nameA nameB a b) =
  MyLetPair ann
    <$> nameToVar nameA <*> nameToVar nameB
      <*> mapVar (p <> [nameA, nameB]) a
      <*> mapVar (p <> [nameA, nameB]) b
mapVar p (MyRecord ann map') = do
  map2 <- traverse (mapVar p) map'
  pure (MyRecord ann map2)
mapVar _ (MyLiteral ann a) = pure (MyLiteral ann a)
mapVar p (MyData ann dt b) =
  MyData ann dt <$> mapVar p b
mapVar _ (MyConstructor ann name) = pure (MyConstructor ann name)
mapVar p (MyConsApp ann fn var) = MyConsApp ann <$> mapVar p fn <*> mapVar p var
mapVar p (MyCaseMatch ann expr' matches catchAll) = do
  let mapVarPair (name, expr'') = (,) name <$> mapVar p expr''
  matches' <- traverse mapVarPair matches
  catchAll' <- traverse (mapVar p) catchAll
  MyCaseMatch ann <$> mapVar p expr' <*> pure matches' <*> pure catchAll'
