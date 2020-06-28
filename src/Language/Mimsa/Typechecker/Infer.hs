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
import Control.Monad.State (State, get, put, runState)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Library
import Language.Mimsa.Syntax
import Language.Mimsa.Types

type App = ExceptT Text (State Int)

type Environment = M.Map Name Scheme

type Substitutions = M.Map Name MonoType

startInference :: Expr -> Either Text MonoType
startInference expr = snd <$> doInference M.empty expr

doInference :: Environment -> Expr -> Either Text (Substitutions, MonoType)
doInference env expr =
  fst either'
  where
    either' = runState (runExceptT (infer env expr)) 1

inferLiteral :: Literal -> App (Substitutions, MonoType)
inferLiteral (MyInt _) = pure (mempty, MTInt)
inferLiteral (MyBool _) = pure (mempty, MTBool)
inferLiteral (MyString _) = pure (mempty, MTString)
inferLiteral (MyUnit) = pure (mempty, MTUnit)

inferBuiltIn :: Name -> App (Substitutions, MonoType)
inferBuiltIn name = case getLibraryFunction name of
  Just ff -> pure (mempty, getFFType ff)
  _ -> throwError $ "Could not find built-in function " <> prettyPrint name

instantiate :: Scheme -> App MonoType
instantiate (Scheme vars ty) = do
  newVars <- traverse (const getUnknown) vars
  let subst = M.fromList (zip vars newVars)
  pure (applySubst subst ty)

applySubstScheme :: Substitutions -> Scheme -> Scheme
applySubstScheme subst (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars (applySubst (foldr M.delete subst vars) t)

applySubstCtx :: Substitutions -> Environment -> Environment
applySubstCtx subst ctx = M.map (applySubstScheme subst) ctx

applySubst :: Substitutions -> MonoType -> MonoType
applySubst subst ty = case ty of
  MTVar i ->
    fromMaybe (MTVar i) (M.lookup i subst)
  MTFunction arg res ->
    MTFunction (applySubst subst arg) (applySubst subst res)
  MTPair a b ->
    MTPair
      (applySubst subst a)
      (applySubst subst b)
  MTList a -> MTList (applySubst subst a)
  MTSum a b -> MTSum (applySubst subst a) (applySubst subst b)
  MTInt -> MTInt
  MTString -> MTString
  MTBool -> MTBool
  MTUnit -> MTUnit

composeSubst :: Substitutions -> Substitutions -> Substitutions
composeSubst s1 s2 = M.union (M.map (applySubst s1) s2) s1

inferVarFromScope :: Environment -> Name -> App (Substitutions, MonoType)
inferVarFromScope env name =
  case M.lookup name env of
    Just scheme -> do
      ty <- instantiate scheme
      pure (mempty, ty)
    _ -> throwError $ T.pack ("Unknown variable " <> show name)

inferFuncReturn :: Environment -> Name -> Expr -> MonoType -> App (Substitutions, MonoType)
inferFuncReturn env binder function tyArg = do
  let scheme = Scheme [] tyArg
      newEnv = M.insert binder scheme env
  tyRes <- getUnknown
  (s1, tyFun) <- infer newEnv function
  s2 <- unify (MTFunction tyArg tyFun) (applySubst s1 tyRes)
  let s3 = mempty
      subs = s3 `composeSubst` s2 `composeSubst` s1
  pure (subs, applySubst subs tyFun)

--traceAnd :: (Show a) => Text -> a -> a
--traceAnd msg a = traceShow (msg, a) a

inferList :: Environment -> NonEmpty Expr -> App (Substitutions, MonoType)
inferList env (a :| as) = do
  (s1, tyA) <- infer env a
  let foldFn = \as' a' -> do
        (s', ty') <- as'
        (sA, tyB) <- infer env a'
        sB <- unify ty' tyB
        pure (sB `composeSubst` sA `composeSubst` s', applySubst sB tyB)
  foldl
    foldFn
    (pure (s1, tyA))
    as

infer :: Environment -> Expr -> App (Substitutions, MonoType)
infer _ (MyLiteral a) = inferLiteral a
infer env (MyVar name) =
  (inferVarFromScope env name)
    <|> (inferBuiltIn name)
infer env (MyList as) = do
  (s1, tyItems) <- inferList env as
  pure (s1, MTList tyItems)
infer env (MyLet binder expr body) = do
  (s1, tyExpr) <- infer env expr
  let scheme = Scheme [] (applySubst s1 tyExpr)
  let newEnv = M.insert binder scheme env
  (s2, tyBody) <- infer (applySubstCtx s1 newEnv) body
  pure (s2 `composeSubst` s1, tyBody)
infer env (MyCase sumExpr (MyLambda binderL exprL) (MyLambda binderR exprR)) = do
  (s1, tySum) <- infer env sumExpr
  case tySum of
    (MTSum tyA tyB) -> do
      (s2, tyLeftRes) <- inferFuncReturn env binderL exprL (applySubst s1 tyA)
      (s3, tyRightRes) <- inferFuncReturn env binderR exprR (applySubst s1 tyB)
      let subs =
            s3
              `composeSubst` s2
              `composeSubst` s1
      s4 <- unify tyLeftRes tyRightRes
      pure (s4 `composeSubst` subs, applySubst (s4 `composeSubst` subs) tyLeftRes)
    (MTVar _a) -> do
      tyVarL <- getUnknown
      tyVarR <- getUnknown
      s2 <- unify (MTSum tyVarL tyVarR) tySum
      (s3, tyLeftRes) <- inferFuncReturn env binderL exprL (applySubst (s2 `composeSubst` s1) tyVarL)
      (s4, tyRightRes) <- inferFuncReturn env binderR exprR (applySubst (s2 `composeSubst` s1) tyVarR)
      let subs =
            s4 `composeSubst` s3
              `composeSubst` s2
              `composeSubst` s1
      s5 <- unify tyLeftRes tyRightRes
      pure (s5 `composeSubst` subs, applySubst (s5 `composeSubst` subs) tyLeftRes)
    a -> throwError $ "Expected to case match on a pair but instead found " <> prettyPrint a
infer _env (MyCase _ _ _) = throwError "Arguments to case match must be lambda functions"
infer env (MyLetPair binder1 binder2 expr body) = do
  (s1, tyExpr) <- infer env expr
  case tyExpr of
    (MTVar _a) -> do
      tyA <- getUnknown
      tyB <- getUnknown
      let schemeA = Scheme [] (applySubst s1 tyA)
          schemeB = Scheme [] (applySubst s1 tyB)
          newEnv = M.insert binder1 schemeA (M.insert binder2 schemeB env)
      s2 <- unify tyExpr (MTPair tyA tyB)
      (s3, tyBody) <- infer (applySubstCtx (s2 `composeSubst` s1) newEnv) body
      pure (s3 `composeSubst` s2 `composeSubst` s1, tyBody)
    (MTPair a b) -> do
      let schemeA = Scheme [] (applySubst s1 a)
          schemeB = Scheme [] (applySubst s1 b)
          newEnv = M.insert binder1 schemeA (M.insert binder2 schemeB env)
      s2 <- unify tyExpr (MTPair a b)
      (s3, tyBody) <- infer (applySubstCtx (s2 `composeSubst` s1) newEnv) body
      pure (s3 `composeSubst` s2 `composeSubst` s1, tyBody)
    a -> throwError $ "Expected a pair but instead found " <> prettyPrint a
infer env (MyLambda binder body) = do
  tyBinder <- getUnknown
  let tmpCtx = M.insert binder (Scheme [] tyBinder) env
  (s1, tyBody) <- infer tmpCtx body
  pure (s1, MTFunction (applySubst s1 tyBinder) tyBody)
infer env (MyApp function argument) = do
  tyRes <- getUnknown
  (s1, tyFun) <- infer env function
  (s2, tyArg) <- infer (applySubstCtx s1 env) argument
  s3 <- unify (applySubst s2 tyFun) (MTFunction tyArg tyRes)
  pure (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tyRes)
infer env (MyIf condition thenCase elseCase) = do
  (s1, tyCond) <- infer env condition
  (s2, tyThen) <- infer (applySubstCtx s1 env) thenCase
  (s3, tyElse) <- infer (applySubstCtx (s2 `composeSubst` s1) env) elseCase
  s4 <- unify tyThen tyElse
  s5 <- unify tyCond MTBool
  let subs =
        s5 `composeSubst` s4 `composeSubst` s3
          `composeSubst` s2
          `composeSubst` s1
  pure
    ( subs,
      applySubst subs tyElse
    )
infer env (MyPair a b) = do
  (s1, tyA) <- infer env a
  (s2, tyB) <- infer (applySubstCtx s1 env) b
  pure (s2 `composeSubst` s1, MTPair tyA tyB)
infer env (MySum MyLeft left') = do
  tyRight <- getUnknown
  (s1, tyLeft) <- infer env left'
  pure (s1, MTSum tyLeft (applySubst s1 tyRight))
infer env (MySum MyRight right') = do
  tyLeft <- getUnknown
  (s1, tyRight) <- infer env right'
  pure (s1, MTSum (applySubst s1 tyLeft) tyRight)

freeTypeVars :: MonoType -> S.Set Name
freeTypeVars ty = case ty of
  MTVar var ->
    S.singleton var
  MTFunction t1 t2 ->
    S.union (freeTypeVars t1) (freeTypeVars t2)
  _ ->
    S.empty

-- | Creates a fresh unification variable and binds it to the given type
varBind :: Name -> MonoType -> App Substitutions
varBind var ty
  | ty == MTVar var = pure mempty
  | S.member var (freeTypeVars ty) =
    throwError $
      prettyPrint var <> " fails occurs check"
  | otherwise = pure (M.singleton var ty)

unify :: MonoType -> MonoType -> App Substitutions
unify a b | a == b = pure mempty
unify (MTFunction l r) (MTFunction l' r') = do
  s1 <- unify l l'
  s2 <- unify (applySubst s1 r) (applySubst s1 r')
  pure (s2 `composeSubst` s1)
unify (MTPair a b) (MTPair a' b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  pure (s2 `composeSubst` s1)
unify (MTSum a b) (MTSum a' b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  pure (s2 `composeSubst` s1)
unify (MTVar u) t = varBind u t
unify t (MTVar u) = varBind u t
unify a b =
  throwError $ T.pack $
    "Unification error: Can't match " <> show a <> " with " <> show b

getUnknown :: App MonoType
getUnknown = do
  nextUniVar <- get
  put (nextUniVar + 1)
  pure (MTVar (mkName $ "U" <> T.pack (show nextUniVar)))
