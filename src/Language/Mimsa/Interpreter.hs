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
interpretWithScope (MyPair a b) = pure (MyPair a b)
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
interpretWithScope (MyVar name) =
  useVarFromBuiltIn name <|> useVarFromScope name
    <|> (throwError $ "Unknown variable " <> prettyPrint name)
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
interpretWithScope (MyApp (MyApp a b) c) = do
  expr <- interpretWithScope (MyApp a b)
  interpretWithScope (MyApp expr c)
interpretWithScope (MyApp (MyLet a b c) d) = do
  expr <- interpretWithScope (MyLet a b c)
  interpretWithScope (MyApp expr d)
interpretWithScope (MyApp (MyLetPair a b c d) e) = do
  expr <- interpretWithScope (MyLetPair a b c d)
  interpretWithScope (MyApp expr e)
interpretWithScope (MySum s a) = do
  expr <- interpretWithScope a
  pure (MySum s expr)
interpretWithScope (MyList a) = pure (MyList a)
interpretWithScope (MyApp (MyList _) _) = throwError "Cannot apply a value to a List"
interpretWithScope (MyApp (MySum MyLeft _) _) = throwError "Cannot apply a value to a Left value"
interpretWithScope (MyApp (MySum MyRight _) _) = throwError "Cannot apply a value to a Right value"
interpretWithScope (MyApp (MyLiteral _) _) = throwError "Cannot apply a value to a literal value"
interpretWithScope (MyApp (MyIf _ _ _) _) = throwError "Cannot apply a value to an if"
interpretWithScope (MyApp (MyPair _ _) _) = throwError "Cannot apply a value to a Pair"
interpretWithScope (MyApp (MyCase _ _ _) _) = throwError "Cannot apply a value to a case match"
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
