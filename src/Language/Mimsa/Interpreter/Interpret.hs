{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.Interpret
  ( interpretWithScope,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Interpreter.PatternMatch
import Language.Mimsa.Interpreter.SwapName
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Library
import Language.Mimsa.Types

-- when we come to do let recursive the name of our binder
-- may already be turned into a number in the expr
-- so we look it up to make sure we bind the right thing
findActualBindingInSwaps :: Int -> App Variable
findActualBindingInSwaps int = do
  swaps <- askForSwaps
  scope' <- readScope
  case M.lookup (NumberedVar int) swaps of
    Just i' -> pure (NamedVar i')
    _ -> throwError $ CouldNotFindVar scope' (NumberedVar int)

useVar :: Variable -> App (Expr Variable)
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
instantiateVar :: Expr Variable -> App (Expr Variable)
instantiateVar expr = case expr of
  (MyLambda binder expr') -> do
    (freshBinder, freshExpr) <- newLambdaCopy binder expr'
    interpretWithScope (MyLambda freshBinder freshExpr)
  other -> interpretWithScope other

runBuiltIn :: BiIds -> ForeignFunc -> App (Expr Variable)
runBuiltIn _ (NoArgs _ io) = liftIO io
runBuiltIn (OneId v1) (OneArg _ io) = do
  expr1 <- useVar v1
  liftIO (io expr1)
runBuiltIn (TwoIds v1 v2) (TwoArgs _ io) = do
  expr1 <- useVar v1
  expr2 <- useVar v2
  liftIO (io expr1 expr2)
runBuiltIn (ThreeIds v1 v2 v3) (ThreeArgs _ io) = do
  expr1 <- useVar v1
  expr2 <- useVar v2
  expr3 <- useVar v3
  liftIO (io expr1 expr2 expr3)
runBuiltIn ids _ = throwError $ CouldNotMatchBuiltInId ids

unwrapBuiltIn :: Name -> ForeignFunc -> App (Expr Variable)
unwrapBuiltIn name (NoArgs _ _) = do
  let actual = BuiltInActual name NoId
  addToScope (Scope $ M.singleton actual (MyVar (BuiltIn name)))
  pure (MyVar actual)
unwrapBuiltIn name (OneArg _ _) = do
  v1 <- nextVariable
  let actual = BuiltInActual name (OneId v1)
  addToScope (Scope $ M.singleton actual (MyVar (BuiltIn name))) -- add new name to scope
  pure
    ( MyLambda v1 (MyVar actual)
    )
unwrapBuiltIn name (TwoArgs _ _) = do
  v1 <- nextVariable
  v2 <- nextVariable
  let actual = BuiltInActual name (TwoIds v1 v2)
  addToScope (Scope $ M.singleton actual (MyVar (BuiltIn name))) -- add new name to scope
  pure
    ( MyLambda
        v1
        ( MyLambda
            v2
            (MyVar actual)
        )
    )
unwrapBuiltIn name (ThreeArgs _ _) = do
  v1 <- nextVariable
  v2 <- nextVariable
  v3 <- nextVariable
  let actual = BuiltInActual name (ThreeIds v1 v2 v3)
  addToScope (Scope $ M.singleton actual (MyVar (BuiltIn name))) -- add new name to scope
  pure
    ( MyLambda
        v1
        ( MyLambda
            v2
            ( MyLambda
                v3
                (MyVar actual)
            )
        )
    )

-- get new var
newLambdaCopy :: Variable -> Expr Variable -> App (Variable, Expr Variable)
newLambdaCopy name expr = do
  newName' <- nextVariable
  newExpr <- swapName name newName' expr
  pure (newName', newExpr)

interpretWithScope :: Expr Variable -> App (Expr Variable)
interpretWithScope interpretExpr =
  case interpretExpr of
    (MyLiteral a) -> pure (MyLiteral a)
    (MyPair a b) -> do
      exprA <- interpretWithScope a
      exprB <- interpretWithScope b
      pure (MyPair exprA exprB)
    (MyLet binder expr body) -> do
      addToScope (Scope $ M.singleton binder expr)
      interpretWithScope body
    (MyLetPair binderA binderB (MyPair a b) body) -> do
      let newScopes = Scope $ M.fromList [(binderA, a), (binderB, b)]
      addToScope newScopes
      interpretWithScope body
    (MyLetPair binderA binderB (MyVar v) body) -> do
      expr <- interpretWithScope (MyVar v)
      interpretWithScope (MyLetPair binderA binderB expr body)
    (MyLetPair _ _ a _) ->
      throwError $ CannotDestructureAsPair a
    (MyVar var) ->
      useVar var >>= interpretWithScope
    (MyApp (MyVar f) value) -> do
      expr <- interpretWithScope (MyVar f)
      interpretWithScope (MyApp expr value)
    (MyApp (MyLambda binder expr) value) -> do
      value' <- interpretWithScope value
      addToScope (Scope $ M.singleton binder value')
      interpretWithScope expr
    (MyApp (MyLiteral a) _) ->
      throwError $ CannotApplyToNonFunction (MyLiteral a)
    (MyApp other value) -> do
      expr <- interpretWithScope other
      interpretWithScope (MyApp expr value)
    (MyRecordAccess (MyRecord record) name) ->
      case M.lookup name record of
        Just item -> interpretWithScope item
        _ -> throwError $ CannotFindMemberInRecord record name
    (MyRecordAccess (MyVar a) name) -> do
      expr <- interpretWithScope (MyVar a)
      interpretWithScope (MyRecordAccess expr name)
    (MyRecordAccess (MyRecordAccess a name') name) -> do
      expr <- interpretWithScope (MyRecordAccess a name')
      interpretWithScope (MyRecordAccess expr name)
    (MyRecordAccess a name) ->
      throwError $ CannotDestructureAsRecord a name
    (MyRecord as) -> do
      exprs <- traverse interpretWithScope as
      pure (MyRecord exprs)
    (MyLambda a b) ->
      pure (MyLambda a b)
    (MyIf (MyLiteral (MyBool pred')) true false) ->
      if pred'
        then interpretWithScope true
        else interpretWithScope false
    (MyIf all'@(MyLiteral _) _ _) ->
      throwError $ PredicateForIfMustBeABoolean all'
    (MyIf all'@(MyLambda _ _) _ _) ->
      throwError $ PredicateForIfMustBeABoolean all'
    (MyIf pred' true false) -> do
      predExpr <- interpretWithScope pred'
      interpretWithScope (MyIf predExpr true false)
    (MyData _dataType expr) -> interpretWithScope expr
    (MyConstructor a) -> pure (MyConstructor a)
    (MyConsApp fn val) -> pure (MyConsApp fn val)
    (MyCaseMatch expr' matches catchAll) -> do
      expr'' <- interpretWithScope expr'
      patternMatch expr'' matches catchAll >>= interpretWithScope
