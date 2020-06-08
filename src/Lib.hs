{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startInference,
    doInference,
    unknowns,
    Expr (..),
    MonoType (..),
    Name,
    mkName,
    UniVar (..),
  )
where

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Types

type Environment = M.Map Name MonoType

startInference :: Expr -> Either Text MonoType
startInference expr = doInference M.empty expr

doInference :: Environment -> Expr -> Either Text MonoType
doInference env expr =
  (fst <$> either')
  where
    either' = runStateT (infer env expr) M.empty

infer :: Environment -> Expr -> App MonoType
infer _ (MyInt _) = pure MTInt
infer _ (MyBool _) = pure MTBool
infer _ (MyString _) = pure MTString
infer env (MyVar name) = case M.lookup name env of
  Just a -> pure a
  _ -> throwError $ T.pack ("Unknown variable " <> show name)
infer env (MyLet binder expr body) = do
  tyExpr <- infer env expr
  let newEnv = M.insert binder tyExpr env
  infer newEnv body
infer env (MyLambda binder body) = do
  tyArg <- getUnknown
  let newEnv = M.insert binder tyArg env
  tyBody <- infer newEnv body
  pure $ MTFunction tyArg tyBody
infer env (MyApp function argument) = do
  tyArg <- infer env argument
  tyFun <- infer env function
  tyRes <- getUnknown
  -- tyFun = tyArg -> tyRes
  _ <- unify tyFun (MTFunction tyArg tyRes)
  apply tyRes
infer env (MyIf condition thenCase elseCase) = do
  tyCond <- infer env condition
  tyThen <- infer env thenCase
  tyElse <- infer env elseCase
  _ <- unify tyCond MTBool
  _ <- unify tyThen tyElse
  pure tyThen

getUniVars :: MonoType -> [UniVar] -> [UniVar]
getUniVars (MTFunction argument result) as = (getUniVars argument as) ++ (getUniVars result as)
getUniVars (MTUnknown a) as = [a] ++ as
getUniVars _ as = as

unknowns :: MonoType -> [UniVar]
unknowns mType = getUniVars mType []

unify :: MonoType -> MonoType -> App ()
unify ty1' ty2' = do
  ty1 <- apply ty1'
  ty2 <- apply ty2'
  unify' ty1 ty2

unify' :: MonoType -> MonoType -> App ()
unify' a b | a == b = pure ()
unify' (MTFunction args result) (MTFunction args' result') = do
  unify args args'
  unify result result'
unify' (MTUnknown i) b = do
  occursCheck i b
  unifyVariable i b
unify' a (MTUnknown i) = do
  occursCheck i a
  unifyVariable i a
unify' a b =
  throwError $ T.pack $
    "Can't match " <> show a <> " with " <> show b

occursCheck :: UniVar -> MonoType -> App ()
occursCheck i mt =
  if (not $ elem i (unknowns mt))
    then pure ()
    else
      throwError $ T.pack $
        "Cannot unify as " <> show (MTUnknown i) <> " occurs within " <> show mt

-- all the Ints we've matched back to types
type Substitutions = M.Map UniVar (Maybe MonoType)

-- replace unknowns with knowns from the substitution list where possible
apply :: MonoType -> App MonoType
apply (MTUnknown i) = do
  sub <- join <$> gets (M.lookup i)
  case sub of
    Just mType -> (apply mType)
    Nothing -> pure (MTUnknown i)
apply (MTFunction args result) =
  MTFunction <$> (apply args) <*> (apply result)
apply other = pure other

type App = StateT Substitutions (Either Text)

getUnknown :: App MonoType
getUnknown = do
  nextUniVar <- gets (\subs -> (M.foldlWithKey (\k k' _ -> max k k') 0 subs) + 1)
  modify (M.insert nextUniVar Nothing)
  pure (MTUnknown nextUniVar)

unifyVariable :: UniVar -> MonoType -> App ()
unifyVariable i mType = modify (M.insert i (Just mType))
-------------------------
--
