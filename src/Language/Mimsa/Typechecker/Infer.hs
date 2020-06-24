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
  a -> a

composeSubst :: Substitutions -> Substitutions -> Substitutions
composeSubst s1 s2 = M.union (M.map (applySubst s1) s2) s1

inferVarFromScope :: Environment -> Name -> App (Substitutions, MonoType)
inferVarFromScope env name =
  case M.lookup name env of
    Just scheme -> do
      ty <- instantiate scheme
      pure (mempty, ty)
    _ -> throwError $ T.pack ("Unknown variable " <> show name)

infer :: Environment -> Expr -> App (Substitutions, MonoType)
infer _ (MyLiteral a) = inferLiteral a
infer env (MyVar name) =
  (inferVarFromScope env name)
    <|> (inferBuiltIn name)
infer env (MyLet binder expr body) = do
  (s1, tyExpr) <- infer env expr
  let scheme = Scheme [] (applySubst s1 tyExpr)
  let newEnv = M.insert binder scheme env
  (s2, tyBody) <- infer (applySubstCtx s1 newEnv) body
  pure (s2 `composeSubst` s1, tyBody)
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
  (s3, tyElse) <- infer (applySubstCtx s2 env) elseCase
  s4 <- unify tyCond MTBool
  s5 <- unify tyThen tyElse
  pure
    ( s5 `composeSubst` s4 `composeSubst` s3
        `composeSubst` s2
        `composeSubst` s1,
      tyThen
    )
infer env (MyPair a b) = do
  (s1, tyA) <- infer env a
  (s2, tyB) <- infer (applySubstCtx s1 env) b
  pure (s2 `composeSubst` s1, MTPair tyA tyB)

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
  s2 <- unify b b'
  pure (s2 `composeSubst` s1)
unify (MTVar u) t = varBind u t
unify t (MTVar u) = varBind u t
unify a b =
  throwError $ T.pack $
    "Can't match " <> show a <> " with " <> show b

getUnknown :: App MonoType
getUnknown = do
  nextUniVar <- get
  put (nextUniVar + 1)
  pure (MTVar (mkName $ "U" <> T.pack (show nextUniVar)))
