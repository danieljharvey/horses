{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.Interpret
  ( interpretWithScope,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Monad.Except
import Data.Functor (($>))
import qualified Data.Map as M
import Language.Mimsa.Interpreter.PatternMatch
import Language.Mimsa.Interpreter.SwapName
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Library
import Language.Mimsa.Types

-- when we come to do let recursive the name of our binder
-- may already be turned into a number in the expr
-- so we look it up to make sure we bind the right thing
findActualBindingInSwaps :: Int -> App ann Variable
findActualBindingInSwaps int = do
  swaps <- askForSwaps
  scope' <- readScope
  case M.lookup (NumberedVar int) swaps of
    Just i' -> pure (NamedVar i')
    _ -> throwError $ CouldNotFindVar scope' (NumberedVar int)

useVar :: (Eq ann, Monoid ann) => Variable -> App ann (Expr Variable ann)
useVar var' = case var' of
  (NumberedVar i) -> do
    scope' <- readScope
    case M.lookup (NumberedVar i) (getScope scope') of
      Just expr -> instantiateVar expr
      Nothing -> do
        var <- findActualBindingInSwaps i -- try it by it's pre-substituted name before failing
        useVar var
  (NamedVar n) -> do
    scope' <- readScope
    case M.lookup (NamedVar n) (getScope scope') of
      Just expr -> instantiateVar expr
      Nothing -> throwError $ CouldNotFindVar scope' (NamedVar n)
  (BuiltIn n) ->
    case getLibraryFunction (BuiltIn n) of
      Just ff -> unwrapBuiltIn n ff
      Nothing -> do
        scope' <- readScope
        throwError $ CouldNotFindBuiltIn scope' (BuiltIn n)
  var@(BuiltInActual n ids) ->
    case getLibraryFunction (BuiltIn n) of
      Just ff -> runBuiltIn ids ff
      Nothing -> do
        scope' <- readScope
        throwError $ CouldNotFindBuiltIn scope' var

-- make a fresh copy for us to use
-- is this necessary?
instantiateVar :: (Eq ann, Monoid ann) => Expr Variable ann -> App ann (Expr Variable ann)
instantiateVar expr = case expr of
  (MyLambda ann binder expr') -> do
    (freshBinder, freshExpr) <- newLambdaCopy binder expr'
    interpretWithScope (MyLambda ann freshBinder freshExpr)
  other -> interpretWithScope other

runBuiltIn ::
  (Eq ann, Monoid ann) =>
  BiIds ->
  ForeignFunc ann ->
  App ann (Expr Variable ann)
runBuiltIn _ (NoArgs _ io) = liftIO io
runBuiltIn (OneId v1) (OneArg _ io) = do
  expr1 <- useVar v1
  liftIO (io expr1)
runBuiltIn ids _ = throwError $ CouldNotMatchBuiltInId ids

unwrapBuiltIn ::
  (Eq ann, Monoid ann) =>
  Name ->
  ForeignFunc ann ->
  App ann (Expr Variable ann)
unwrapBuiltIn name (NoArgs _ _) = do
  let actual = BuiltInActual name NoId
  addToScope (Scope $ M.singleton actual (MyVar mempty (BuiltIn name)))
  pure (MyVar mempty actual)
unwrapBuiltIn name (OneArg _ _) = do
  v1 <- nextVariable
  let actual = BuiltInActual name (OneId v1)
  addToScope (Scope $ M.singleton actual (MyVar mempty (BuiltIn name))) -- add new name to scope
  pure
    ( MyLambda mempty v1 (MyVar mempty actual)
    )

-- get new var
newLambdaCopy :: Variable -> Expr Variable ann -> App ann (Variable, Expr Variable ann)
newLambdaCopy name expr = do
  newName' <- nextVariable
  newExpr <- swapName name newName' expr
  pure (newName', newExpr)

interpretOperator ::
  (Eq ann, Monoid ann) =>
  Operator ->
  Expr Variable ann ->
  Expr Variable ann ->
  App ann (Expr Variable ann)
interpretOperator Equals a b =
  let plainA = a $> ()
      plainB = b $> ()
      respondWith = pure . MyLiteral mempty . MyBool
   in if plainA == plainB
        then respondWith True
        else respondWith False

interpretWithScope :: (Eq ann, Monoid ann) => Expr Variable ann -> App ann (Expr Variable ann)
interpretWithScope interpretExpr =
  case interpretExpr of
    (MyLiteral ann a) -> pure (MyLiteral ann a)
    (MyPair ann a b) -> do
      exprA <- interpretWithScope a
      exprB <- interpretWithScope b
      pure (MyPair ann exprA exprB)
    (MyLet _ binder expr body) -> do
      addToScope (Scope $ M.singleton binder expr)
      interpretWithScope body
    (MyLetPair _ binderA binderB (MyPair _ a b) body) -> do
      let newScopes = Scope $ M.fromList [(binderA, a), (binderB, b)]
      addToScope newScopes
      interpretWithScope body
    (MyLetPair ann binderA binderB (MyVar ann' v) body) -> do
      expr <- interpretWithScope (MyVar ann' v)
      interpretWithScope (MyLetPair ann binderA binderB expr body)
    (MyLetPair _ _ _ a _) ->
      throwError $ CannotDestructureAsPair a
    (MyInfix _ op a b) -> interpretOperator op a b
    (MyVar _ var) ->
      useVar var >>= interpretWithScope
    (MyApp ann (MyVar ann' f) value) -> do
      expr <- interpretWithScope (MyVar ann' f)
      interpretWithScope (MyApp ann expr value)
    (MyApp _ (MyLambda _ binder expr) value) -> do
      value' <- interpretWithScope value
      addToScope (Scope $ M.singleton binder value')
      interpretWithScope expr
    (MyApp _ (MyLiteral ann a) _) ->
      throwError $ CannotApplyToNonFunction (MyLiteral ann a)
    (MyApp ann other value) -> do
      expr <- interpretWithScope other
      interpretWithScope (MyApp ann expr value)
    (MyRecordAccess _ (MyRecord _ record) name) ->
      case M.lookup name record of
        Just item -> interpretWithScope item
        _ -> throwError $ CannotFindMemberInRecord record name
    (MyRecordAccess ann (MyVar ann' a) name) -> do
      expr <- interpretWithScope (MyVar ann' a)
      interpretWithScope (MyRecordAccess ann expr name)
    (MyRecordAccess ann (MyRecordAccess ann' a name') name) -> do
      expr <- interpretWithScope (MyRecordAccess ann' a name')
      interpretWithScope (MyRecordAccess ann expr name)
    (MyRecordAccess _ a name) ->
      throwError $ CannotDestructureAsRecord a name
    (MyRecord ann as) -> do
      exprs <- traverse interpretWithScope as
      pure (MyRecord ann exprs)
    (MyLambda ann a b) ->
      pure (MyLambda ann a b)
    (MyIf _ (MyLiteral _ (MyBool pred')) true false) ->
      if pred'
        then interpretWithScope true
        else interpretWithScope false
    (MyIf _ all'@(MyLiteral _ _) _ _) ->
      throwError $ PredicateForIfMustBeABoolean all'
    (MyIf _ all'@MyLambda {} _ _) ->
      throwError $ PredicateForIfMustBeABoolean all'
    (MyIf ann pred' true false) -> do
      predExpr <- interpretWithScope pred'
      interpretWithScope (MyIf ann predExpr true false)
    (MyData _ _ expr) -> interpretWithScope expr
    (MyConstructor ann a) -> pure (MyConstructor ann a)
    (MyConsApp ann fn val) ->
      MyConsApp ann fn <$> interpretWithScope val
    (MyCaseMatch _ expr' matches catchAll) -> do
      expr'' <- interpretWithScope expr'
      patternMatch expr'' matches catchAll >>= interpretWithScope
