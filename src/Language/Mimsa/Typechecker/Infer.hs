{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Mimsa.Typechecker.Infer
  ( startInference,
    doInference,
    doDataTypeInference,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Language.Mimsa.Library
import Language.Mimsa.Typechecker.DataTypes
  ( builtInTypes,
    defaultEnv,
  )
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.Patterns (checkCompleteness)
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types

startInference ::
  (Eq ann, Monoid ann) =>
  Swaps ->
  Expr ann Variable ->
  Either (TypeError ann) MonoType
startInference swaps expr = snd <$> doInference swaps mempty expr

doInference ::
  (Eq ann, Monoid ann) =>
  Swaps ->
  Environment ->
  Expr ann Variable ->
  Either (TypeError ann) (Substitutions, MonoType)
doInference swaps env expr = runTcMonad swaps (inferAndSubst (defaultEnv <> env) expr)

doDataTypeInference ::
  Environment ->
  DataType ->
  Either (TypeError ann) (Map TyCon TypeConstructor)
doDataTypeInference env dt =
  runTcMonad mempty (snd <$> inferConstructorTypes (defaultEnv <> env) dt)

-- run inference, and substitute everything possible
inferAndSubst ::
  (Eq ann, Monoid ann) =>
  Environment ->
  Expr ann Variable ->
  TcMonad ann (Substitutions, MonoType)
inferAndSubst env expr = do
  (s, tyExpr) <- infer env expr
  pure (s, applySubst s tyExpr)

applySubstCtx :: Substitutions -> Environment -> Environment
applySubstCtx subst (Environment schemes dt) =
  Environment (M.map (applySubstScheme subst) schemes) dt

applySubstScheme :: Substitutions -> Scheme -> Scheme
applySubstScheme (Substitutions subst) (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars (applySubst newSubst t)
  where
    newSubst = Substitutions $ foldr M.delete subst vars

instantiate :: Scheme -> TcMonad ann (Substitutions, MonoType)
instantiate (Scheme vars ty) = do
  newVars <- traverse (const getUnknown) vars
  let subst = Substitutions $ M.fromList (zip vars newVars)
  pure (subst, applySubst subst ty)

--------------

inferLiteral :: Literal -> TcMonad ann (Substitutions, MonoType)
inferLiteral (MyInt _) = pure (mempty, MTInt)
inferLiteral (MyBool _) = pure (mempty, MTBool)
inferLiteral (MyString _) = pure (mempty, MTString)
inferLiteral MyUnit = pure (mempty, MTUnit)

inferBuiltIn ::
  forall ann.
  (Eq ann, Monoid ann) =>
  Variable ->
  TcMonad ann (Substitutions, MonoType)
inferBuiltIn name = case getLibraryFunction @ann name of
  Just ff -> instantiate (generalise mempty (getFFType ff))
  _ -> throwError $ MissingBuiltIn name

inferVarFromScope ::
  Environment ->
  Variable ->
  TcMonad ann (Substitutions, MonoType)
inferVarFromScope env name =
  let lookup' name' (Environment env' _) = M.lookup name' env'
   in case lookup' name env of
        Just mt ->
          instantiate mt
        _ -> do
          swaps <- ask
          throwError $
            VariableNotInEnv
              swaps
              name
              (S.fromList (M.keys (getSchemes env)))

createEnv :: Variable -> Scheme -> Environment
createEnv binder scheme =
  Environment (M.singleton binder scheme) mempty

splitRecordTypes ::
  Map Name (Substitutions, MonoType) ->
  (Substitutions, MonoType)
splitRecordTypes map' = (subs, MTRecord types)
  where
    subs =
      mconcat (fst . snd <$> M.toList map')
    types = snd <$> map'

-- let's pattern match on exactly what's inside more clearly
inferApplication ::
  (Eq ann, Monoid ann) =>
  Environment ->
  Expr ann Variable ->
  Expr ann Variable ->
  TcMonad ann (Substitutions, MonoType)
inferApplication env function argument = do
  tyRes <- getUnknown
  (s1, tyFun) <- infer env function
  (s2, tyArg) <- infer (applySubstCtx s1 env) argument
  s3 <- unify (applySubst s2 tyFun) (MTFunction tyArg tyRes)
  pure (s3 <> s2 <> s1, applySubst s3 tyRes)

-- when we come to do let recursive the name of our binder
-- may already be turned into a number in the expr
-- so we look it up to make sure we bind the right thing
findActualBindingInSwaps :: Variable -> TcMonad ann Variable
findActualBindingInSwaps (NamedVar var) = do
  swaps <- ask
  case listToMaybe $ M.keys $ M.filter (== var) swaps of
    Just i -> pure i
    _ -> pure (NamedVar var)
findActualBindingInSwaps a = pure a

-- to allow recursion we make a type for the let binding in it's own expression
-- we may need to unify tyUnknown and tyExpr if it struggles with complex stuff
inferLetBinding ::
  (Eq ann, Monoid ann) =>
  Environment ->
  Variable ->
  Expr ann Variable ->
  Expr ann Variable ->
  TcMonad ann (Substitutions, MonoType)
inferLetBinding env binder expr body = do
  tyUnknown <- getUnknown
  binderInExpr <- findActualBindingInSwaps binder
  let newEnv1 = createEnv binderInExpr (Scheme mempty tyUnknown) <> env
  (s1, tyExpr) <- infer newEnv1 expr
  let newEnv2 = createEnv binder (generalise s1 tyExpr) <> newEnv1
  (s2, tyBody) <- infer (applySubstCtx s1 newEnv2) body
  s3 <- unify tyUnknown tyExpr
  pure (s3 <> s2 <> s1, applySubst s3 tyBody)

inferLetPairBinding ::
  (Eq ann, Monoid ann) =>
  Environment ->
  Variable ->
  Variable ->
  Expr ann Variable ->
  Expr ann Variable ->
  TcMonad ann (Substitutions, MonoType)
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

-- given a datatype declaration, checks it makes sense and if so,
-- add it to the Environment
storeDataDeclaration ::
  (Eq ann, Monoid ann) =>
  Environment ->
  DataType ->
  Expr ann Variable ->
  TcMonad ann (Substitutions, MonoType)
storeDataDeclaration env dt@(DataType tyName _ _) expr' =
  if M.member tyName (getDataTypes env)
    then throwError (DuplicateTypeDeclaration tyName)
    else
      let newEnv = Environment mempty (M.singleton tyName dt)
       in infer (newEnv <> env) expr'

-- infer the type of a data constructor
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferDataConstructor :: Environment -> TyCon -> TcMonad ann (Substitutions, MonoType)
inferDataConstructor env name = do
  dataType <- lookupConstructor env name
  (_, allArgs) <- inferConstructorTypes env dataType
  case M.lookup name allArgs of
    Just tyArg ->
      pure (mempty, constructorToType tyArg)
    Nothing -> throwError UnknownTypeError -- shouldn't happen (but will)

-- infer types for data type and it's constructor in one big go
inferConstructorTypes ::
  Environment ->
  DataType ->
  TcMonad ann (MonoType, Map TyCon TypeConstructor)
inferConstructorTypes env (DataType typeName tyNames constructors) = do
  tyVars <- traverse (\a -> (,) a <$> getUnknown) tyNames
  let findType ty = case ty of
        ConsName cn vs -> do
          vs' <- traverse findType vs
          inferType env cn vs'
        VarName var ->
          case filter (\(tyName, _) -> tyName == var) tyVars of
            [(_, tyFound)] -> pure tyFound
            _ -> throwError $ TypeVariableNotInDataType typeName var (fst <$> tyVars)
  let inferConstructor (consName, tyArgs) = do
        tyCons <- traverse findType tyArgs
        let constructor = TypeConstructor typeName (snd <$> tyVars) tyCons
        pure $ M.singleton consName constructor
  cons' <- traverse inferConstructor (M.toList constructors)
  let dt = MTData typeName (snd <$> tyVars)
  pure (dt, mconcat cons')

-- parse a type from it's name
-- this will soon become insufficient for more complex types
inferType :: Environment -> TyCon -> [MonoType] -> TcMonad ann MonoType
inferType env tyName tyVars =
  case M.lookup tyName (getDataTypes env) of
    (Just _) -> case lookupBuiltIn tyName of
      Just mt -> pure mt
      _ -> pure (MTData tyName tyVars)
    _ -> throwError (TypeConstructorNotInScope env tyName)

lookupBuiltIn :: TyCon -> Maybe MonoType
lookupBuiltIn name = M.lookup name builtInTypes

-----

-- check a list of types are all the same
matchList :: [MonoType] -> TcMonad ann (Substitutions, MonoType)
matchList =
  foldl
    ( \ty' tyB' -> do
        (sA, tyA) <- ty'
        sB <- unify tyA tyB'
        pure (sA <> sB, applySubst (sA <> sB) tyB')
    )
    ( (,) mempty
        <$> getUnknown
    )

-----

constructorToType :: TypeConstructor -> MonoType
constructorToType (TypeConstructor typeName tyVars constructTypes) =
  foldr MTFunction (MTData typeName tyVars) constructTypes

inferSumExpressionType ::
  (Eq ann, Monoid ann) =>
  Environment ->
  Map TyCon TypeConstructor ->
  Expr ann Variable ->
  TcMonad ann (Substitutions, MonoType)
inferSumExpressionType env consTypes sumExpr =
  let fromName name =
        case M.lookup name consTypes of
          Just tyCons -> do
            (s1, tySum) <- infer env sumExpr
            s2 <- unify (unwind (constructorToType tyCons)) tySum
            pure
              ( s2 <> s1,
                applySubst s2 tySum
              )
          Nothing -> throwError $ CannotCaseMatchOnType sumExpr
      unwind tyExpr = case tyExpr of
        MTFunction _ b -> unwind b
        a -> a
      findConstructor expr = case expr of
        (MyConsApp _ a _) -> findConstructor a
        (MyConstructor _ a) -> fromName a
        somethingElse ->
          infer env somethingElse
   in findConstructor sumExpr

-------------

inferCaseMatch ::
  (Eq ann, Monoid ann) =>
  Environment ->
  Expr ann Variable ->
  NonEmpty (TyCon, Expr ann Variable) ->
  Maybe (Expr ann Variable) ->
  TcMonad ann (Substitutions, MonoType)
inferCaseMatch env sumExpr matches catchAll = do
  dataType <- checkCompleteness env matches catchAll
  (tyData, constructTypes) <- inferConstructorTypes env dataType
  (s1, tySum) <-
    inferSumExpressionType
      env
      constructTypes
      sumExpr
  s2 <- unify tySum tyData
  tyMatches <-
    traverse
      ( uncurry
          ( inferMatch
              (applySubstCtx (s2 <> s1) env)
              (applySubstToConstructor (s2 <> s1) <$> constructTypes)
          )
      )
      matches
  (sCatch, tyCatches) <- case catchAll of
    Just catchAll' -> do
      (s, tyCatchAll) <- infer (applySubstCtx (s2 <> s1) env) catchAll'
      pure (s, [tyCatchAll])
    _ -> pure (mempty, mempty)
  let matchSubs =
        mconcat
          (fst <$> NE.toList tyMatches)
          <> sCatch
      actuals = (snd <$> NE.toList tyMatches) <> tyCatches
  (s, mt) <- matchList actuals
  let allSubs = s2 <> s1 <> s <> matchSubs
  pure (allSubs, applySubst allSubs mt)

applySubstToConstructor :: Substitutions -> TypeConstructor -> TypeConstructor
applySubstToConstructor subs (TypeConstructor name ty b) =
  TypeConstructor name (applySubst subs <$> ty) (applySubst subs <$> b)

-- infer the type of a case match function
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferMatch ::
  (Eq ann, Monoid ann) =>
  Environment ->
  Map TyCon TypeConstructor ->
  TyCon ->
  Expr ann Variable ->
  TcMonad ann (Substitutions, MonoType)
inferMatch env constructTypes name expr' =
  case M.lookup name constructTypes of
    Nothing -> throwError UnknownTypeError
    Just (TypeConstructor _ _ tyArgs) ->
      case tyArgs of
        [] -> infer env expr' -- no arguments to pass to expr'
        _ -> do
          (s1, tyExpr) <- infer env expr' -- expression return
          (s2, tyRes) <- applyList tyArgs tyExpr
          let subs = s2 <> s1
          pure (subs, applySubst subs tyRes)

applyList :: [MonoType] -> MonoType -> TcMonad ann (Substitutions, MonoType)
applyList vars tyFun = case vars of
  [] -> pure (mempty, tyFun)
  (var : vars') -> do
    tyRes <- getUnknown
    s1 <-
      unify
        (MTFunction var tyRes)
        tyFun
    (s2, tyFun') <- applyList vars' (applySubst s1 tyRes)
    pure (s2 <> s1, applySubst (s2 <> s1) tyFun')

infer ::
  (Eq ann, Monoid ann) =>
  Environment ->
  Expr ann Variable ->
  TcMonad ann (Substitutions, MonoType)
infer env inferExpr =
  case inferExpr of
    (MyLiteral _ a) -> inferLiteral a
    (MyVar _ name) ->
      inferVarFromScope env name
        <|> inferBuiltIn name
    (MyRecord _ map') -> do
      tyRecord <- getUnknown
      (s1, tyResult) <- splitRecordTypes <$> traverse (infer env) map'
      s2 <- unify tyResult tyRecord
      pure
        ( s2 <> s1,
          applySubst (s2 <> s1) tyRecord
        )
    (MyLet _ binder expr body) ->
      inferLetBinding env binder expr body
    (MyRecordAccess _ (MyRecord _ items') name) ->
      case M.lookup name items' of
        Just item ->
          infer env item
        Nothing ->
          throwError $ MissingRecordMember name (S.fromList (M.keys items'))
    (MyRecordAccess _ a name) -> do
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
    (MyLetPair _ binder1 binder2 expr body) ->
      inferLetPairBinding env binder1 binder2 expr body
    (MyLambda _ binder body) -> do
      tyBinder <- getUnknown
      let tmpCtx =
            createEnv binder (Scheme [] tyBinder)
              <> env
      (s1, tyBody) <- infer tmpCtx body
      pure (s1, MTFunction (applySubst s1 tyBinder) tyBody)
    (MyApp _ function argument) -> inferApplication env function argument
    (MyIf _ condition thenCase elseCase) -> do
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
    (MyPair _ a b) -> do
      (s1, tyA) <- infer env a
      (s2, tyB) <- infer env b
      let subs = s2 <> s1
      pure (subs, MTPair tyA tyB)
    (MyData _ dataType expr) ->
      storeDataDeclaration env dataType expr
    (MyConstructor _ name) ->
      inferDataConstructor env name
    (MyConsApp _ cons val) ->
      inferApplication env cons val
    (MyCaseMatch _ expr' matches catchAll) ->
      inferCaseMatch env expr' matches catchAll
