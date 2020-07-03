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
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Library
import Language.Mimsa.Syntax
import Language.Mimsa.Types

interpret :: Scope -> Expr -> IO (Either Text Expr)
interpret scope' expr = do
  ((fmap . fmap) fst either')
  where
    either' = runExceptT $ runStateT (interpretWithScope expr) scope'

type App = StateT Scope (ExceptT Text IO)

useVarFromScope :: Name -> App Expr
useVarFromScope name = do
  found <- gets (M.lookup name . getScope)
  case found of
    Just expr -> interpretWithScope expr
    Nothing -> throwError $ "Could not find " <> prettyPrint name

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
      Nothing -> throwError $ "Could not find built-in function " <> prettyPrint name
    Just unwrappedName -> case getLibraryFunction unwrappedName of
      Just ff -> runBuiltIn unwrappedName ff
      Nothing -> throwError $ "Could not find built-in function " <> prettyPrint name

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
  throwError $ "Cannot destructure value " <> prettyPrint a <> " as a pair"
interpretWithScope (MyVar name) = do
  scope <- get
  useVarFromBuiltIn name <|> useVarFromScope name
    <|> (throwError $ "Unknown variable " <> prettyPrint name <> " in " <> (T.pack $ show scope))
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
interpretWithScope (MyCase a _ _) = throwError $ "Cannot case match on " <> prettyPrint a
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
    _ -> throwError $ "Could not find " <> prettyPrint name <> " in " <> prettyPrint (MyRecord record)
interpretWithScope (MyRecordAccess (MyVar a) name) = do
  expr <- interpretWithScope (MyVar a)
  interpretWithScope (MyRecordAccess expr name)
interpretWithScope (MyRecordAccess (MyRecordAccess a name') name) = do
  expr <- interpretWithScope (MyRecordAccess a name')
  interpretWithScope (MyRecordAccess expr name)
interpretWithScope (MyRecordAccess a name) =
  throwError $
    "Could not access record item " <> prettyPrint name
      <> " inside "
      <> prettyPrint a
interpretWithScope (MyLetList binderHead binderRest (MyVar b) body) = do
  expr <- interpretWithScope (MyVar b)
  interpretWithScope (MyLetList binderHead binderRest expr body)
interpretWithScope (MyLetList _ _ a _) =
  throwError $ "Cannot destructure value " <> prettyPrint a <> " as a list"
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
  throwError $ "Cannot apply a value to " <> prettyPrint thing
interpretWithScope (MyLambda a b) = pure (MyLambda a b)
interpretWithScope (MyIf (MyLiteral (MyBool pred')) true false) =
  if pred'
    then interpretWithScope true
    else interpretWithScope false
interpretWithScope (MyIf (MyLiteral _) _ _) = throwError "Predicate for If must be a Boolean"
interpretWithScope (MyIf (MyLambda _ _) _ _) = throwError "Predicate for If must be a Boolean"
interpretWithScope (MyIf pred' true false) = do
  predExpr <- interpretWithScope pred'
  interpretWithScope (MyIf predExpr true false)
