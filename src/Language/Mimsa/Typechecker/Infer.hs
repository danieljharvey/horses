{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Infer
  ( startInference,
    doInference,
  )
where

import Control.Applicative
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Language.Mimsa.Library
import Language.Mimsa.Typechecker.Instantiate
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types

startInference ::
  Swaps ->
  Expr Variable ->
  Either TypeError MonoType
startInference swaps expr = snd <$> doInference swaps M.empty expr

doInference ::
  Swaps ->
  Environment ->
  Expr Variable ->
  Either TypeError (Substitutions, MonoType)
doInference swaps env expr = runTcMonad swaps (infer env expr)

applySubstCtx :: Substitutions -> Environment -> Environment
applySubstCtx subst ctx = M.map (applySubst subst) ctx

--------------

inferLiteral :: Literal -> TcMonad (Substitutions, MonoType)
inferLiteral (MyInt _) = pure (mempty, MTInt)
inferLiteral (MyBool _) = pure (mempty, MTBool)
inferLiteral (MyString _) = pure (mempty, MTString)
inferLiteral (MyUnit) = pure (mempty, MTUnit)

inferBuiltIn :: Variable -> TcMonad (Substitutions, MonoType)
inferBuiltIn name = case getLibraryFunction name of
  Just ff -> pure (mempty, getFFType ff)
  _ -> do
    throwError $ MissingBuiltIn name

inferVarFromScope :: Environment -> Variable -> TcMonad (Substitutions, MonoType)
inferVarFromScope env name =
  case M.lookup name env of
    Just mt -> do
      clearType <- instantiate mt
      pure (mempty, clearType)
    _ -> do
      throwError $ VariableNotInEnv name (S.fromList (M.keys env))

inferFuncReturn ::
  Environment ->
  Variable ->
  Expr Variable ->
  MonoType ->
  TcMonad (Substitutions, MonoType)
inferFuncReturn env binder function tyArg = do
  let newEnv = M.insert binder tyArg env
  tyRes <- getUnknown
  (s1, tyFun) <- infer newEnv function
  s2 <- unify (MTFunction tyArg tyFun) (applySubst s1 tyRes)
  let s3 = mempty
      subs = s3 <> s2 <> s1
  pure (subs, applySubst subs tyFun)

inferList :: Environment -> NonEmpty (Expr Variable) -> TcMonad (Substitutions, MonoType)
inferList env (a :| as) = do
  (s1, tyA) <- infer env a
  let foldFn = \as' a' -> do
        (s', ty') <- as'
        (sA, tyB) <- infer env a'
        sB <- unify ty' tyB
        pure (sB <> sA <> s', applySubst sB tyB)
  foldl
    foldFn
    (pure (s1, tyA))
    as

splitRecordTypes ::
  Map Name (Substitutions, MonoType) ->
  (Substitutions, MonoType)
splitRecordTypes map' = (subs, MTRecord types)
  where
    subs =
      foldr
        (<>)
        mempty
        ((fst . snd) <$> M.toList map')
    types = snd <$> map'

createScheme :: Environment -> Variable -> MonoType -> Environment
createScheme env binder tyBinder = M.insert binder tyBinder env

infer :: Environment -> (Expr Variable) -> TcMonad (Substitutions, MonoType)
infer _ (MyLiteral a) = inferLiteral a
infer env (MyVar name) =
  (inferVarFromScope env name)
    <|> (inferBuiltIn name)
infer env (MyList as) = do
  (s1, tyItems) <- inferList env as
  pure (s1, MTList tyItems)
infer env (MyRecord map') = do
  tyRecord <- getUnknown
  (s1, tyResult) <- splitRecordTypes <$> traverse (infer env) map'
  s2 <- unify tyResult tyRecord
  pure
    ( s2 <> s1,
      applySubst (s2 <> s1) tyRecord
    )
infer env (MyLet binder expr body) = do
  (s1, tyExpr) <- infer env expr
  let scheme = (applySubst s1 tyExpr)
  let newEnv = traceShowId $ M.insert binder scheme env
  (s2, tyBody) <- infer (applySubstCtx s1 newEnv) body
  pure (s2 <> s1, tyBody)
infer env (MyRecordAccess (MyRecord items') name) = do
  case M.lookup name items' of
    Just item -> do
      infer env item
    Nothing -> do
      throwError $ MissingRecordMember name (S.fromList (M.keys items'))
infer env (MyRecordAccess a name) = do
  (s1, tyItems) <- infer env a
  tyResult <- case tyItems of
    (MTRecord bits) -> do
      case M.lookup name bits of
        Just mt -> pure mt
        _ -> do
          throwError $ MissingRecordTypeMember name bits
    (MTVar _) -> getUnknown
    _ -> throwError $ CannotMatchRecord env tyItems
  s2 <-
    unify
      (MTRecord $ M.singleton name tyResult)
      tyItems
  let subs = s2 <> s1
  pure (subs, applySubst subs tyResult)
infer env (MyCase sumExpr (MyLambda binderL exprL) (MyLambda binderR exprR)) = do
  (s1, tySum) <- infer env sumExpr
  (tyL, tyR) <- case tySum of
    (MTSum tyL tyR) -> pure (tyL, tyR)
    (MTVar _a) -> do
      tyL <- getUnknown
      tyR <- getUnknown
      pure (tyL, tyR)
    otherType -> throwError $ CaseMatchExpectedSum otherType
  s2 <- unify (MTSum tyL tyR) tySum
  (s3, tyLeftRes) <- inferFuncReturn env binderL exprL (applySubst (s2 <> s1) tyL)
  (s4, tyRightRes) <- inferFuncReturn env binderR exprR (applySubst (s2 <> s1) tyR)
  let subs = s4 <> s3 <> s2 <> s1
  s5 <- unify tyLeftRes tyRightRes
  pure (s5 <> subs, applySubst (s5 <> subs) tyLeftRes)
infer _env (MyCase _ l r) = throwError $ CaseMatchExpectedLambda l r
infer env (MyLetPair binder1 binder2 expr body) = do
  (s1, tyExpr) <- infer env expr
  (tyA, tyB) <- case tyExpr of
    (MTVar _a) -> do
      tyA <- getUnknown
      tyB <- getUnknown
      pure (tyA, tyB)
    (MTPair a b) -> pure (a, b)
    a -> throwError $ CaseMatchExpectedPair a
  let schemeA = (applySubst s1 tyA)
      schemeB = (applySubst s1 tyB)
      newEnv = M.insert binder1 schemeA (M.insert binder2 schemeB env)
  s2 <- unify tyExpr (MTPair tyA tyB)
  (s3, tyBody) <- infer (applySubstCtx (s2 <> s1) newEnv) body
  pure (s3 <> s2 <> s1, tyBody)
infer env (MyLetList binder1 binder2 expr body) = do
  (s1, tyExpr) <- infer env expr
  (tyHead, tyRest) <- case tyExpr of
    (MTVar _a) -> do
      tyHead <- getUnknown
      tyRest <- getUnknown
      pure (tyHead, tyRest)
    (MTList tyHead) -> do
      tyRest <- getUnknown
      pure (tyHead, tyRest)
    a -> throwError $ CaseMatchExpectedList a
  let schemeHead = (applySubst s1 tyHead)
      schemeRest = (applySubst s1 tyRest)
      newEnv = M.insert binder1 schemeHead (M.insert binder2 schemeRest env)
  s2 <- unify tyExpr (MTList tyHead)
  s3 <- unify tyRest (MTSum MTUnit (MTList tyHead))
  (s4, tyBody) <- infer (applySubstCtx (s3 <> s2 <> s1) newEnv) body
  pure (s4 <> s3 <> s2 <> s1, tyBody)
infer env (MyLambda binder body) = do
  tyBinder <- getUnknown
  let tmpCtx = createScheme env binder tyBinder
  (s1, tyBody) <- infer tmpCtx body
  pure (s1, MTFunction (applySubst s1 tyBinder) tyBody)
infer env (MyForAllLambda binder body) = do
  tyBinder <- getUnknown
  let tmpCtx = createScheme env binder tyBinder
  (s1, tyBody) <- infer tmpCtx body
  pure (s1, MTFunction (applySubst s1 tyBinder) tyBody)
infer env (MyApp function argument) = do
  tyRes <- getUnknown
  (s1, tyFun) <- infer env function
  (s2, tyArg) <- infer (applySubstCtx s1 env) argument
  s3 <- unify (traceShowId $ applySubst s2 tyFun) (MTFunction tyArg tyRes)
  pure (s3 <> s2 <> s1, applySubst (s3 <> s2 <> s1) tyRes)
infer env (MyIf condition thenCase elseCase) = do
  (s1, tyCond) <- infer env condition
  (s2, tyThen) <- infer (applySubstCtx s1 env) thenCase
  (s3, tyElse) <- infer (applySubstCtx (s2 <> s1) env) elseCase
  s4 <- unify tyThen tyElse
  s5 <- unify tyCond MTBool
  let subs = s5 <> s4 <> s3 <> s2 <> s1
  pure
    ( subs,
      applySubst subs tyElse
    )
infer env (MyPair a b) = do
  (s1, tyA) <- infer env a
  (s2, tyB) <- infer env b
  let subs = (s2 <> s1)
  pure (subs, traceShowId $ MTPair tyA tyB)
infer env (MySum MyLeft left') = do
  tyRight <- getUnknown
  (s1, tyLeft) <- infer env left'
  pure (s1, MTSum tyLeft (applySubst s1 tyRight))
infer env (MySum MyRight right') = do
  tyLeft <- getUnknown
  (s1, tyRight) <- infer env right'
  pure (s1, MTSum (applySubst s1 tyLeft) tyRight)
