{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Infer
  ( startInference,
    doInference,
  )
where

import Control.Applicative
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Library
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types

startInference ::
  Swaps ->
  Expr Variable ->
  Either TypeError MonoType
startInference swaps expr = snd <$> doInference swaps mempty expr

doInference ::
  Swaps ->
  Environment ->
  Expr Variable ->
  Either TypeError (Substitutions, MonoType)
doInference swaps env expr = runTcMonad swaps (infer env expr)

applySubstCtx :: Substitutions -> Environment -> Environment
applySubstCtx subst (Environment schemes dt) =
  Environment (M.map (applySubstScheme subst) schemes) dt

applySubstScheme :: Substitutions -> Scheme -> Scheme
applySubstScheme (Substitutions subst) (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars (applySubst newSubst t)
  where
    newSubst = Substitutions $ foldr M.delete subst vars

instantiate :: Scheme -> TcMonad (Substitutions, MonoType)
instantiate (Scheme vars ty) = do
  newVars <- traverse (const getUnknown) vars
  let subst = Substitutions $ M.fromList (zip vars newVars)
  pure (subst, applySubst subst ty)

--------------

inferLiteral :: Literal -> TcMonad (Substitutions, MonoType)
inferLiteral (MyInt _) = pure (mempty, MTInt)
inferLiteral (MyBool _) = pure (mempty, MTBool)
inferLiteral (MyString _) = pure (mempty, MTString)
inferLiteral MyUnit = pure (mempty, MTUnit)

inferBuiltIn :: Variable -> TcMonad (Substitutions, MonoType)
inferBuiltIn name = case getLibraryFunction name of
  Just ff -> instantiate (generalise mempty (getFFType ff))
  _ -> throwError $ MissingBuiltIn name

inferVarFromScope ::
  Environment ->
  Variable ->
  TcMonad (Substitutions, MonoType)
inferVarFromScope env name =
  let lookup' name' (Environment env' _) = M.lookup name' env'
   in case lookup' name env of
        Just mt ->
          instantiate mt
        _ ->
          throwError $
            VariableNotInEnv
              name
              (S.fromList (M.keys (getSchemes env)))

createEnv :: Variable -> Scheme -> Environment
createEnv binder scheme =
  Environment (M.singleton binder scheme) mempty

inferFuncReturn ::
  Environment ->
  Variable ->
  Expr Variable ->
  MonoType ->
  TcMonad (Substitutions, MonoType)
inferFuncReturn env binder function tyArg = do
  let newEnv = createEnv binder (Scheme [] tyArg) <> env
  tyRes <- getUnknown
  (s1, tyFun) <- infer newEnv function
  s2 <- unify (MTFunction tyArg tyFun) (applySubst s1 tyRes)
  let s3 = mempty
      subs = s3 <> s2 <> s1
  pure (subs, applySubst subs tyFun)

inferList :: Environment -> NonEmpty (Expr Variable) -> TcMonad (Substitutions, MonoType)
inferList env (a :| as) = do
  (s1, tyA) <- infer env a
  let foldFn as' a' = do
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
      mconcat (fst . snd <$> M.toList map')
    types = snd <$> map'

-- let's pattern match on exactly what's inside more clearly
inferApplication :: Environment -> Expr Variable -> Expr Variable -> TcMonad (Substitutions, MonoType)
inferApplication env function argument = do
  tyRes <- getUnknown
  (s1, tyFun) <- infer env function
  (s2, tyArg) <- infer (applySubstCtx s1 env) argument
  s3 <- unify (applySubst s2 tyFun) (MTFunction tyArg tyRes)
  pure (s3 <> s2 <> s1, applySubst s3 tyRes)

inferLetBinding :: Environment -> Variable -> Expr Variable -> Expr Variable -> TcMonad (Substitutions, MonoType)
inferLetBinding env binder expr body = do
  (s1, tyExpr) <- infer env expr
  let newEnv = createEnv binder (generalise s1 tyExpr) <> env
  (s2, tyBody) <- infer (applySubstCtx s1 newEnv) body
  pure (s2 <> s1, tyBody)

inferLetListBinding ::
  Environment ->
  Variable ->
  Variable ->
  Expr Variable ->
  Expr Variable ->
  TcMonad (Substitutions, MonoType)
inferLetListBinding env binder1 binder2 expr body = do
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
  let schemeHead = Scheme mempty (applySubst s1 tyHead)
      schemeRest = Scheme mempty (applySubst s1 tyRest)
      newEnv = createEnv binder1 schemeHead <> createEnv binder2 schemeRest <> env
  s2 <- unify tyExpr (MTList tyHead)
  s3 <- unify tyRest (MTSum MTUnit (MTList tyHead))
  (s4, tyBody) <- infer (applySubstCtx (s3 <> s2 <> s1) newEnv) body
  pure (s4 <> s3 <> s2 <> s1, tyBody)

inferLetPairBinding ::
  Environment ->
  Variable ->
  Variable ->
  Expr Variable ->
  Expr Variable ->
  TcMonad (Substitutions, MonoType)
inferLetPairBinding env binder1 binder2 expr body = do
  (s1, tyExpr) <- infer env expr
  (tyA, tyB) <- case tyExpr of
    (MTVar _a) -> do
      tyA <- getUnknown
      tyB <- getUnknown
      pure (tyA, tyB)
    (MTPair a b) -> pure (a, b)
    a -> throwError $ CaseMatchExpectedPair a
  let schemeA = Scheme mempty (applySubst s1 tyA)
      schemeB = Scheme mempty (applySubst s1 tyB)
      newEnv = createEnv binder1 schemeA <> createEnv binder2 schemeB <> env
  s2 <- unify tyExpr (MTPair tyA tyB)
  (s3, tyBody) <- infer (applySubstCtx (s2 <> s1) newEnv) body
  pure (s3 <> s2 <> s1, tyBody)

inferDataDeclaration ::
  Environment ->
  Construct ->
  NonEmpty (Construct, [Construct]) ->
  Expr Variable ->
  TcMonad (Substitutions, MonoType)
inferDataDeclaration env tyName constructors' expr' =
  let newEnv = Environment mempty (M.singleton tyName constructors')
   in infer (newEnv <> env) expr'

inferDataConstructor :: Environment -> Construct -> TcMonad (Substitutions, MonoType)
inferDataConstructor env name =
  let hasMatchingConstructor :: NonEmpty (Construct, a) -> Bool
      hasMatchingConstructor =
        (> 0) . length . NE.filter (\(const', _) -> const' == name)
   in case M.toList $ M.filter hasMatchingConstructor (getDataTypes env) of
        ((tyName, _) : _) -> pure (mempty, MTConstructor tyName)
        _ -> throwError (TypeConstructorNotInScope env name)

infer :: Environment -> Expr Variable -> TcMonad (Substitutions, MonoType)
infer env inferExpr =
  case inferExpr of
    (MyLiteral a) -> inferLiteral a
    (MyVar name) ->
      inferVarFromScope env name
        <|> inferBuiltIn name
    (MyList as) -> do
      (s1, tyItems) <- inferList env as
      pure (s1, MTList tyItems)
    (MyRecord map') -> do
      tyRecord <- getUnknown
      (s1, tyResult) <- splitRecordTypes <$> traverse (infer env) map'
      s2 <- unify tyResult tyRecord
      pure
        ( s2 <> s1,
          applySubst (s2 <> s1) tyRecord
        )
    (MyLet binder expr body) ->
      inferLetBinding env binder expr body
    (MyRecordAccess (MyRecord items') name) ->
      case M.lookup name items' of
        Just item ->
          infer env item
        Nothing ->
          throwError $ MissingRecordMember name (S.fromList (M.keys items'))
    (MyRecordAccess a name) -> do
      (s1, tyItems) <- infer env a
      tyResult <- case tyItems of
        (MTRecord bits) ->
          case M.lookup name bits of
            Just mt -> pure mt
            _ ->
              throwError $ MissingRecordTypeMember name bits
        (MTVar _) -> getUnknown
        _ -> throwError $ CannotMatchRecord env tyItems
      s2 <-
        unify
          (MTRecord $ M.singleton name tyResult)
          tyItems
      let subs = s2 <> s1
      pure (subs, applySubst subs tyResult)
    (MyCase sumExpr (MyLambda binderL exprL) (MyLambda binderR exprR)) -> do
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
    (MyCase _ l r) -> throwError $ CaseMatchExpectedLambda l r
    (MyLetPair binder1 binder2 expr body) ->
      inferLetPairBinding env binder1 binder2 expr body
    (MyLetList binder1 binder2 expr body) ->
      inferLetListBinding env binder1 binder2 expr body
    (MyLambda binder body) -> do
      tyBinder <- getUnknown
      let tmpCtx =
            createEnv binder (Scheme [] tyBinder)
              <> env
      (s1, tyBody) <- infer tmpCtx body
      pure (s1, MTFunction (applySubst s1 tyBinder) tyBody)
    (MyApp function argument) -> inferApplication env function argument
    (MyIf condition thenCase elseCase) -> do
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
    (MyPair a b) -> do
      (s1, tyA) <- infer env a
      (s2, tyB) <- infer env b
      let subs = s2 <> s1
      pure (subs, MTPair tyA tyB)
    (MySum MyLeft left') -> do
      tyRight <- getUnknown
      (s1, tyLeft) <- infer env left'
      pure (s1, MTSum tyLeft (applySubst s1 tyRight))
    (MySum MyRight right') -> do
      tyLeft <- getUnknown
      (s1, tyRight) <- infer env right'
      pure (s1, MTSum (applySubst s1 tyLeft) tyRight)
    (MyData tyName constructors expr) ->
      inferDataDeclaration env tyName constructors expr
    (MyConstructor name) -> inferDataConstructor env name
