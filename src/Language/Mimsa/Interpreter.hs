{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter
  ( interpret,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Syntax
import Language.Mimsa.Types hiding (Scope (..))

interpret :: Expr -> Either Text Expr
interpret expr =
  (fst <$> either')
  where
    either' = runStateT (interpretWithScope expr) M.empty

type Scope = Map Name Expr

type App = StateT Scope (Either Text)

interpretWithScope :: Expr -> App Expr
interpretWithScope (MyBool a) = pure $ MyBool a
interpretWithScope (MyInt a) = pure $ MyInt a
interpretWithScope (MyString a) = pure $ MyString a
interpretWithScope (MyLet binder expr body) = do
  modify ((<>) (M.singleton binder expr))
  interpretWithScope body
interpretWithScope (MyVar a) = do
  found <- gets (M.lookup a)
  case found of
    Just expr -> pure expr
    Nothing -> throwError $ "Could not find " <> prettyPrint a
interpretWithScope (MyApp (MyVar f) value) = do
  expr <- interpretWithScope (MyVar f)
  interpretWithScope (MyApp expr value)
interpretWithScope (MyApp (MyLambda binder expr) value) =
  interpretWithScope (MyLet binder expr value)
interpretWithScope (MyLambda a b) = pure (MyLambda a b)
interpretWithScope (MyApp _ _) = throwError "Can only apply a value to a lambda"
interpretWithScope (MyIf (MyBool pred') true false) =
  if pred'
    then interpretWithScope true
    else interpretWithScope false
interpretWithScope (MyIf (MyString _) _ _) = throwError "Predicate for If must be a Boolean"
interpretWithScope (MyIf (MyInt _) _ _) = throwError "Predicate for If must be a Boolean"
interpretWithScope (MyIf (MyLambda _ _) _ _) = throwError "Predicate for If must be a Boolean"
interpretWithScope (MyIf pred' true false) = do
  predExpr <- interpretWithScope pred'
  interpretWithScope (MyIf predExpr true false)
