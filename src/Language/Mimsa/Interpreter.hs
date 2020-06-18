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
import Data.Coerce
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Library
import Language.Mimsa.Syntax
import Language.Mimsa.Types

interpret :: Scope -> Expr -> IO (Either Text Expr)
interpret scope' expr =
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

useVarFromBuiltIn :: Name -> App Expr
useVarFromBuiltIn name = do
  case M.lookup (coerce name) libraryFunctions of
    Just (_, io) -> do
      expr <- liftIO io
      pure expr
    Nothing -> throwError $ "Could not find built-in function " <> prettyPrint name

interpretWithScope :: Expr -> App Expr
interpretWithScope (MyLiteral a) = pure $ MyLiteral a
interpretWithScope (MyLet binder expr body) = do
  modify ((<>) (Scope $ M.singleton binder expr))
  interpretWithScope body
interpretWithScope (MyVar name) =
  useVarFromBuiltIn name <|> useVarFromScope name
    <|> (throwError $ "Unknown variable " <> prettyPrint name)
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
interpretWithScope (MyApp (MyLiteral _) _) = throwError "Cannot apply a value to a literal value"
interpretWithScope (MyApp (MyIf _ _ _) _) = throwError "Cannot apply a value to an if"
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
