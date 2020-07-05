{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter
  ( interpret,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Library
import Language.Mimsa.Types

interpret :: Scope -> Expr -> IO (Either InterpreterError Expr)
interpret scope' expr = do
  result <- either'
  pure (fmap fst result)
  where
    either' =
      runExceptT $
        runStateT
          (interpretWithScope expr)
          (scope')

type App = StateT Scope (ExceptT InterpreterError IO)

useVarFromScope :: Name -> App Expr
useVarFromScope name = do
  found <- gets (M.lookup name . getScope)
  case found of
    Just expr -> do
      case expr of
        (MyLambda binder expr') -> do
          (freshBinder, freshExpr) <- newLambdaCopy binder expr'
          interpretWithScope (MyLambda freshBinder freshExpr)
        other -> interpretWithScope other
    Nothing -> do
      scope' <- get
      throwError $ CouldNotFindVar scope' name

wrappedName :: Name -> Name
wrappedName (Name n) = Name (n <> "__unwrapped")

wrappedVarName :: Name -> Int -> Name
wrappedVarName name i = Name $ wrapped' <> (T.pack $ show i)
  where
    (Name wrapped') = wrappedName name

unwrap :: Name -> Maybe Name
unwrap (Name n) = Name <$> T.stripSuffix "__unwrapped" n

-- return lambda to new function, with __unwrapped in the name
-- if Name ends in __unwrapped then run it else return new function...
useVarFromBuiltIn :: Name -> App Expr
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

runBuiltIn :: Name -> ForeignFunc -> App Expr
runBuiltIn _ (NoArgs _ io) = liftIO io
runBuiltIn name (OneArg _ io) = do
  expr1 <- useVarFromScope (wrappedVarName name 1)
  liftIO (io expr1)
runBuiltIn name (TwoArgs _ io) = do
  expr1 <- useVarFromScope (wrappedVarName name 1)
  expr2 <- useVarFromScope (wrappedVarName name 2)
  liftIO (io expr1 expr2)

unwrapBuiltIn :: Name -> ForeignFunc -> App Expr
unwrapBuiltIn name (NoArgs t' io) = runBuiltIn name (NoArgs t' io)
unwrapBuiltIn name (OneArg _ _) = do
  let wrapped = wrappedName name -- rename our foreign func
  modify ((<>) (Scope $ M.singleton wrapped (MyVar name))) -- add new name to scope
  pure
    ( MyLambda
        (wrappedVarName name 1)
        (MyVar wrapped)
    )
unwrapBuiltIn name (TwoArgs _ _) = do
  let wrapped = wrappedName name
  modify ((<>) (Scope $ M.singleton wrapped (MyVar name))) -- add new name to scope
  pure
    ( MyLambda
        (wrappedVarName name 1)
        (MyLambda (wrappedVarName name 2) (MyVar wrapped))
    )

-- get new var
newLambdaCopy :: Name -> Expr -> App (Name, Expr)
newLambdaCopy name expr = do
  newName' <- newName
  newExpr <- swapName name newName' expr
  pure (newName', newExpr)

newName :: App Name
newName = do
  let makeName :: Int -> Name
      makeName i = mkName $ "var" <> T.pack (show i)
  makeName <$> gets (M.size . getScope)

-- step through Expr, replacing vars with numbered variables
swapName :: Name -> Name -> Expr -> App Expr
swapName from to (MyVar from') =
  pure $
    if from == from'
      then MyVar to
      else MyVar from'
swapName from to (MyLet name a b) =
  MyLet <$> pure name <*> (swapName from to a)
    <*> (swapName from to b)
swapName from to (MyLambda name a) =
  MyLambda <$> pure name <*> (swapName from to a)
swapName from to (MyRecordAccess a name) =
  MyRecordAccess <$> (swapName from to a) <*> pure name
swapName from to (MyApp a b) =
  MyApp <$> (swapName from to a)
    <*> (swapName from to b)
swapName from to (MyIf a b c) =
  MyIf
    <$> (swapName from to a)
      <*> (swapName from to b)
      <*> (swapName from to c)
swapName from to (MyPair a b) =
  MyPair
    <$> (swapName from to a) <*> (swapName from to b)
swapName from to (MyLetPair nameA nameB a b) =
  MyLetPair
    <$> pure nameA <*> pure nameB
      <*> (swapName from to a)
      <*> (swapName from to b)
swapName from to (MyLetList nameHead nameRest a b) =
  MyLetList <$> pure nameHead
    <*> pure nameRest
    <*> (swapName from to a)
    <*> (swapName from to b)
swapName from to (MySum side a) =
  MySum
    <$> pure side
      <*> (swapName from to a)
swapName from to (MyCase a b c) =
  MyCase <$> (swapName from to a) <*> (swapName from to b)
    <*> (swapName from to c)
swapName from to (MyList as) = do
  mas <- traverse (swapName from to) as
  pure (MyList mas)
swapName from to (MyRecord map') = do
  map2 <- traverse (swapName from to) map'
  pure (MyRecord map2)
swapName _ _ (MyLiteral a) = pure (MyLiteral a)

interpretWithScope :: Expr -> App Expr
interpretWithScope (MyLiteral a) = pure (MyLiteral a)
interpretWithScope (MyPair a b) = do
  exprA <- interpretWithScope a
  exprB <- interpretWithScope b
  pure (MyPair exprA exprB)
interpretWithScope (MyLet binder expr body) = do
  modify ((<>) (Scope $ M.singleton binder expr))
  interpretWithScope body
interpretWithScope (MyLetPair binderA binderB (MyPair a b) body) = do
  let newScopes = Scope $ M.fromList [(binderA, a), (binderB, b)]
  modify ((<>) newScopes)
  interpretWithScope body
interpretWithScope (MyLetPair binderA binderB (MyVar v) body) = do
  expr <- interpretWithScope (MyVar v)
  interpretWithScope (MyLetPair binderA binderB expr body)
interpretWithScope (MyLetPair _ _ a _) =
  throwError $ CannotDestructureAsPair a
interpretWithScope (MyVar name) = do
  scope <- get
  useVarFromBuiltIn name <|> useVarFromScope name
    <|> (throwError $ CouldNotFindVar scope name)
interpretWithScope (MyCase (MySum MyLeft a) (MyLambda binderL exprL) _) = do
  interpretWithScope (MyLet binderL a exprL)
interpretWithScope (MyCase (MySum MyRight b) _ (MyLambda binderR exprR)) = do
  interpretWithScope (MyLet binderR b exprR)
interpretWithScope (MyCase (MyVar a) l r) = do
  expr <- interpretWithScope (MyVar a)
  interpretWithScope (MyCase expr l r)
interpretWithScope (MyCase (MyApp a b) l r) = do
  expr <- interpretWithScope (MyApp a b)
  interpretWithScope (MyCase expr l r)
interpretWithScope (MyCase a _ _) = throwError $ CannotDestructureAsSum a
interpretWithScope (MyApp (MyVar f) value) = do
  expr <- interpretWithScope (MyVar f)
  interpretWithScope (MyApp expr value)
interpretWithScope (MyApp (MyLambda binder expr) value) =
  interpretWithScope (MyLet binder value expr)
interpretWithScope (MyLetList binderHead binderRest (MyList as) body) = do
  let (listHead, listTail) = NE.uncons as
      tail' = case listTail of
        Nothing -> MySum MyLeft (MyLiteral MyUnit)
        Just bs -> MySum MyRight (MyList bs)
  let newScopes = Scope $ M.fromList [(binderHead, listHead), (binderRest, tail')]
  modify ((<>) newScopes)
  interpretWithScope body
interpretWithScope (MyRecordAccess (MyRecord record) name) = do
  case M.lookup name record of
    Just item -> interpretWithScope item
    _ -> throwError $ CannotFindMemberInRecord record name
interpretWithScope (MyRecordAccess (MyVar a) name) = do
  expr <- interpretWithScope (MyVar a)
  interpretWithScope (MyRecordAccess expr name)
interpretWithScope (MyRecordAccess (MyRecordAccess a name') name) = do
  expr <- interpretWithScope (MyRecordAccess a name')
  interpretWithScope (MyRecordAccess expr name)
interpretWithScope (MyRecordAccess a name) =
  throwError $ CannotDestructureAsRecord a name
interpretWithScope (MyLetList binderHead binderRest (MyVar b) body) = do
  expr <- interpretWithScope (MyVar b)
  interpretWithScope (MyLetList binderHead binderRest expr body)
interpretWithScope (MyLetList _ _ a _) =
  throwError $ CannotDestructureAsList a
interpretWithScope (MyApp (MyApp a b) c) = do
  expr <- interpretWithScope (MyApp a b)
  interpretWithScope (MyApp expr c)
interpretWithScope (MyApp (MyLet a b c) d) = do
  expr <- interpretWithScope (MyLet a b c)
  interpretWithScope (MyApp expr d)
interpretWithScope (MyApp (MyLetPair a b c d) e) = do
  expr <- interpretWithScope (MyLetPair a b c d)
  interpretWithScope (MyApp expr e)
interpretWithScope (MyApp (MyRecordAccess a b) c) = do
  expr <- interpretWithScope (MyRecordAccess a b)
  interpretWithScope (MyApp expr c)
interpretWithScope (MySum s a) = do
  expr <- interpretWithScope a
  pure (MySum s expr)
interpretWithScope (MyApp (MyLetList a b c d) e) = do
  expr <- interpretWithScope (MyLetList a b c d)
  interpretWithScope (MyApp expr e)
interpretWithScope (MyList as) = do
  exprs <- traverse interpretWithScope as
  pure (MyList exprs)
interpretWithScope (MyRecord as) = do
  exprs <- traverse interpretWithScope as
  pure (MyRecord exprs)
interpretWithScope (MyApp thing _) =
  throwError $ CannotApplyToNonFunction thing
interpretWithScope (MyLambda a b) = pure (MyLambda a b)
interpretWithScope (MyIf (MyLiteral (MyBool pred')) true false) =
  if pred'
    then interpretWithScope true
    else interpretWithScope false
interpretWithScope (MyIf all'@(MyLiteral _) _ _) =
  throwError $ PredicateForIfMustBeABoolean all'
interpretWithScope (MyIf all'@(MyLambda _ _) _ _) =
  throwError $ PredicateForIfMustBeABoolean all'
interpretWithScope (MyIf pred' true false) = do
  predExpr <- interpretWithScope pred'
  interpretWithScope (MyIf predExpr true false)
