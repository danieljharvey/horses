{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.Interpret
  ( interpretWithScope,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Language.Mimsa.Interpreter.BuiltIns
import Language.Mimsa.Interpreter.SwapName
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Library
import Language.Mimsa.Types

useVarFromScope :: Variable -> App (Expr Variable)
useVarFromScope name = do
  found <- gets (M.lookup name . getScope)
  case found of
    Just expr ->
      case expr of
        (MyLambda binder expr') -> do
          (freshBinder, freshExpr) <- newLambdaCopy binder expr'
          interpretWithScope (MyLambda freshBinder freshExpr)
        other -> interpretWithScope other
    Nothing -> do
      scope' <- get
      throwError $ CouldNotFindVar scope' name

-- return lambda to new function, with __unwrapped in the name
-- if Name ends in __unwrapped then run it else return new function...
useVarFromBuiltIn :: Variable -> App (Expr Variable)
useVarFromBuiltIn name =
  case unwrap name of
    Nothing -> case getLibraryFunction name of
      Just ff -> unwrapBuiltIn name ff
      Nothing -> do
        scope' <- get
        throwError $ CouldNotFindBuiltIn scope' name
    Just unwrappedName -> case getLibraryFunction unwrappedName of
      Just ff -> runBuiltIn unwrappedName ff
      Nothing -> do
        scope' <- get
        throwError $ CouldNotFindBuiltIn scope' name

runBuiltIn :: Variable -> ForeignFunc -> App (Expr Variable)
runBuiltIn _ (NoArgs _ io) = liftIO io
runBuiltIn name (OneArg _ io) = do
  expr1 <- useVarFromScope (wrappedVarName name 1)
  liftIO (io expr1)
runBuiltIn name (TwoArgs _ io) = do
  expr1 <- useVarFromScope (wrappedVarName name 1)
  expr2 <- useVarFromScope (wrappedVarName name 2)
  liftIO (io expr1 expr2)
runBuiltIn name (ThreeArgs _ io) = do
  expr1 <- useVarFromScope (wrappedVarName name 1)
  expr2 <- useVarFromScope (wrappedVarName name 2)
  expr3 <- useVarFromScope (wrappedVarName name 3)
  liftIO (io expr1 expr2 expr3)

unwrapBuiltIn :: Variable -> ForeignFunc -> App (Expr Variable)
unwrapBuiltIn name (NoArgs t' io) = runBuiltIn name (NoArgs t' io)
unwrapBuiltIn name (OneArg _ _) = do
  let wrapped = wrappedName name -- rename our foreign func
  addToScope (Scope $ M.singleton wrapped (MyVar name)) -- add new name to scope
  pure
    ( MyLambda
        (wrappedVarName name 1)
        (MyVar wrapped)
    )
unwrapBuiltIn name (TwoArgs _ _) = do
  let wrapped = wrappedName name
  addToScope (Scope $ M.singleton wrapped (MyVar name)) -- add new name to scope
  pure
    ( MyLambda
        (wrappedVarName name 1)
        ( MyLambda
            (wrappedVarName name 2)
            (MyVar wrapped)
        )
    )
unwrapBuiltIn name (ThreeArgs _ _) = do
  let wrapped = wrappedName name
  addToScope (Scope $ M.singleton wrapped (MyVar name)) -- add new name to scope
  pure
    ( MyLambda
        (wrappedVarName name 1)
        ( MyLambda
            (wrappedVarName name 2)
            ( MyLambda
                (wrappedVarName name 3)
                (MyVar wrapped)
            )
        )
    )

-- get new var
newLambdaCopy :: Variable -> Expr Variable -> App (Variable, Expr Variable)
newLambdaCopy name expr = do
  newName' <- newName
  newExpr <- swapName name newName' expr
  pure (newName', newExpr)

newName :: App Variable
newName =
  NumberedVar <$> gets (M.size . getScope)

addToScope :: Scope -> App ()
addToScope scope' = modify $ (<>) scope'

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
    (MyVar name) -> do
      scope <- get
      (useVarFromBuiltIn name >>= interpretWithScope)
        <|> (useVarFromScope name >>= interpretWithScope)
        <|> throwError (CouldNotFindVar scope name)
    (MyCase (MySum MyLeft a) (MyLambda binderL exprL) _) ->
      interpretWithScope (MyLet binderL a exprL)
    (MyCase (MySum MyRight b) _ (MyLambda binderR exprR)) ->
      interpretWithScope (MyLet binderR b exprR)
    (MyCase (MyVar a) l r) -> do
      expr <- interpretWithScope (MyVar a)
      interpretWithScope (MyCase expr l r)
    (MyCase (MyApp a b) l r) -> do
      expr <- interpretWithScope (MyApp a b)
      interpretWithScope (MyCase expr l r)
    (MyCase a _ _) -> throwError $ CannotDestructureAsSum a
    (MyApp (MyVar f) value) -> do
      expr <- interpretWithScope (MyVar f)
      interpretWithScope (MyApp expr value)
    (MyApp (MyLambda binder expr) value) ->
      interpretWithScope (MyLet binder value expr)
    (MyApp other value) -> do
      expr <- interpretWithScope other
      interpretWithScope (MyApp expr value)
    (MyLetList binderHead binderRest (MyList as) body) -> do
      let (listHead, listTail) = NE.uncons as
          tail' = case listTail of
            Nothing -> MySum MyLeft (MyLiteral MyUnit)
            Just bs -> MySum MyRight (MyList bs)
      let newScopes = Scope $ M.fromList [(binderHead, listHead), (binderRest, tail')]
      addToScope newScopes
      interpretWithScope body
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
    (MyLetList binderHead binderRest (MyVar b) body) -> do
      expr <- interpretWithScope (MyVar b)
      interpretWithScope (MyLetList binderHead binderRest expr body)
    (MyLetList _ _ a _) ->
      throwError $ CannotDestructureAsList a
    (MySum s a) -> do
      expr <- interpretWithScope a
      pure (MySum s expr)
    (MyList as) -> do
      exprs <- traverse interpretWithScope as
      pure (MyList exprs)
    (MyRecord as) -> do
      exprs <- traverse interpretWithScope as
      pure (MyRecord exprs)
    (MyLambda a b) -> pure (MyLambda a b)
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
