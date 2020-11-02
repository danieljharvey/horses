{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Infer
  ( startInference,
    doInference,
    doDataTypeInference,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Language.Mimsa.Typechecker.DataTypes
  ( builtInTypes,
    defaultEnv,
  )
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.Patterns (checkCompleteness)
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

startInference ::
  Swaps ->
  Expr Variable Annotation ->
  Either TypeError MonoType
startInference swaps expr = snd <$> doInference swaps mempty expr

doInference ::
  Swaps ->
  Environment ->
  Expr Variable Annotation ->
  Either TypeError (Substitutions, MonoType)
doInference swaps env expr = runTcMonad swaps (inferAndSubst (defaultEnv <> env) expr)

doDataTypeInference ::
  Environment ->
  DataType ->
  Either TypeError (Map TyCon TypeConstructor)
doDataTypeInference env dt =
  runTcMonad mempty (snd <$> inferConstructorTypes (defaultEnv <> env) dt)

-- run inference, and substitute everything possible
inferAndSubst ::
  Environment ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
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

instantiate :: Scheme -> TcMonad (Substitutions, MonoType)
instantiate (Scheme vars ty) = do
  newVars <- traverse (const $ getUnknown mempty) vars
  let subst = Substitutions $ M.fromList (zip vars newVars)
  pure (subst, applySubst subst ty)

--------------

inferLiteral :: Annotation -> Literal -> TcMonad (Substitutions, MonoType)
inferLiteral ann (MyInt _) = pure (mempty, MTPrim ann MTInt)
inferLiteral ann (MyBool _) = pure (mempty, MTPrim ann MTBool)
inferLiteral ann (MyString _) = pure (mempty, MTPrim ann MTString)
inferLiteral ann MyUnit = pure (mempty, MTPrim ann MTUnit)

inferVarFromScope ::
  Environment ->
  Annotation ->
  Variable ->
  TcMonad (Substitutions, MonoType)
inferVarFromScope env ann name =
  let lookup' name' (Environment env' _) = M.lookup name' env'
   in case lookup' name env of
        Just mt ->
          instantiate mt
        _ -> do
          swaps <- ask
          throwError $
            VariableNotInEnv
              swaps
              ann
              name
              (S.fromList (M.keys (getSchemes env)))

createEnv :: Variable -> Scheme -> Environment
createEnv binder scheme =
  Environment (M.singleton binder scheme) mempty

splitRecordTypes ::
  Map Name (Substitutions, MonoType) ->
  (Substitutions, MonoType)
splitRecordTypes map' = (subs, MTRecord mempty types)
  where
    subs =
      mconcat (fst . snd <$> M.toList map')
    types = snd <$> map'

-- let's pattern match on exactly what's inside more clearly
inferApplication ::
  Environment ->
  Annotation ->
  Expr Variable Annotation ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
inferApplication env ann function argument = do
  tyRes <- getUnknown ann
  (s1, tyFun) <- infer env function
  (s2, tyArg) <- infer (applySubstCtx s1 env) argument
  s3 <- unify (applySubst s2 tyFun) (MTFunction ann tyArg tyRes)
  pure (s3 <> s2 <> s1, applySubst s3 tyRes)

-- when we come to do let recursive the name of our binder
-- may already be turned into a number in the expr
-- so we look it up to make sure we bind the right thing
findActualBindingInSwaps :: Variable -> TcMonad Variable
findActualBindingInSwaps (NamedVar var) = do
  swaps <- ask
  case listToMaybe $ M.keys $ M.filter (== var) swaps of
    Just i -> pure i
    _ -> pure (NamedVar var)
findActualBindingInSwaps a = pure a

-- to allow recursion we make a type for the let binding in it's own expression
-- we may need to unify tyUnknown and tyExpr if it struggles with complex stuff
inferLetBinding ::
  Environment ->
  Annotation ->
  Variable ->
  Expr Variable Annotation ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
inferLetBinding env ann binder expr body = do
  tyUnknown <- getUnknown ann
  binderInExpr <- findActualBindingInSwaps binder
  let newEnv1 = createEnv binderInExpr (Scheme mempty tyUnknown) <> env
  (s1, tyExpr) <- infer newEnv1 expr
  let newEnv2 = createEnv binder (generalise s1 tyExpr) <> newEnv1
  (s2, tyBody) <- infer (applySubstCtx s1 newEnv2) body
  s3 <- unify tyUnknown tyExpr
  pure (s3 <> s2 <> s1, applySubst s3 tyBody)

inferLetPairBinding ::
  Environment ->
  Variable ->
  Variable ->
  Expr Variable Annotation ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
inferLetPairBinding env binder1 binder2 expr body = do
  (s1, tyExpr) <- infer env expr
  (tyA, tyB) <- case tyExpr of
    (MTVar ann _a) -> do
      tyA <- getUnknown ann
      tyB <- getUnknown ann
      pure (tyA, tyB)
    (MTPair _ a b) -> pure (a, b)
    a -> throwError $ CaseMatchExpectedPair (getAnnotation expr) a
  let schemeA = Scheme mempty (applySubst s1 tyA)
      schemeB = Scheme mempty (applySubst s1 tyB)
      newEnv = createEnv binder1 schemeA <> createEnv binder2 schemeB <> env
  s2 <- unify tyExpr (MTPair mempty tyA tyB)
  (s3, tyBody) <- infer (applySubstCtx (s2 <> s1) newEnv) body
  pure (s3 <> s2 <> s1, tyBody)

-- given a datatype declaration, checks it makes sense and if so,
-- add it to the Environment
storeDataDeclaration ::
  Environment ->
  DataType ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
storeDataDeclaration env dt@(DataType tyName _ _) expr' =
  if M.member tyName (getDataTypes env)
    then throwError (DuplicateTypeDeclaration tyName)
    else
      let newEnv = Environment mempty (M.singleton tyName dt)
       in infer (newEnv <> env) expr'

-- infer the type of a data constructor
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferDataConstructor :: Environment -> Annotation -> TyCon -> TcMonad (Substitutions, MonoType)
inferDataConstructor env ann name = do
  dataType <- lookupConstructor env ann name
  (_, allArgs) <- inferConstructorTypes env dataType
  case M.lookup name allArgs of
    Just tyArg ->
      pure (mempty, constructorToType tyArg)
    Nothing -> throwError UnknownTypeError -- shouldn't happen (but will)

-- infer types for data type and it's constructor in one big go
inferConstructorTypes ::
  Environment ->
  DataType ->
  TcMonad (MonoType, Map TyCon TypeConstructor)
inferConstructorTypes env (DataType typeName tyNames constructors) = do
  tyVars <- traverse (\a -> (,) a <$> getUnknown mempty) tyNames
  let findType ty = case ty of
        ConsName cn vs -> do
          vs' <- traverse findType vs
          inferType env mempty cn vs'
        VarName var ->
          case filter (\(tyName, _) -> tyName == var) tyVars of
            [(_, tyFound)] -> pure tyFound
            _ -> throwError $ TypeVariableNotInDataType typeName var (fst <$> tyVars)
  let inferConstructor (consName, tyArgs) = do
        tyCons <- traverse findType tyArgs
        let constructor = TypeConstructor typeName (snd <$> tyVars) tyCons
        pure $ M.singleton consName constructor
  cons' <- traverse inferConstructor (M.toList constructors)
  let dt = MTData mempty typeName (snd <$> tyVars)
  pure (dt, mconcat cons')

-- parse a type from it's name
-- this will soon become insufficient for more complex types
inferType :: Environment -> Annotation -> TyCon -> [MonoType] -> TcMonad MonoType
inferType env ann tyName tyVars =
  case M.lookup tyName (getDataTypes env) of
    (Just _) -> case lookupBuiltIn tyName of
      Just mt -> pure mt
      _ -> pure (MTData mempty tyName tyVars)
    _ -> throwError (TypeConstructorNotInScope env ann tyName)

lookupBuiltIn :: TyCon -> Maybe MonoType
lookupBuiltIn name = M.lookup name builtInTypes

-----

-- check a list of types are all the same
matchList :: [MonoType] -> TcMonad (Substitutions, MonoType)
matchList =
  foldl
    ( \ty' tyB' -> do
        (sA, tyA) <- ty'
        sB <- unify tyA tyB'
        pure (sA <> sB, applySubst (sA <> sB) tyB')
    )
    ( (,) mempty
        <$> getUnknown mempty
    )

-----

constructorToType :: TypeConstructor -> MonoType
constructorToType (TypeConstructor typeName tyVars constructTypes) =
  foldr (MTFunction mempty) (MTData mempty typeName tyVars) constructTypes

inferSumExpressionType ::
  Environment ->
  Map TyCon TypeConstructor ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
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
        MTFunction _ _ b -> unwind b
        a -> a
      findConstructor expr = case expr of
        (MyConsApp _ a _) -> findConstructor a
        (MyConstructor _ a) -> fromName a
        somethingElse ->
          infer env somethingElse
   in findConstructor sumExpr

-------------

inferCaseMatch ::
  Environment ->
  Annotation ->
  Expr Variable Annotation ->
  NonEmpty (TyCon, Expr Variable Annotation) ->
  Maybe (Expr Variable Annotation) ->
  TcMonad (Substitutions, MonoType)
inferCaseMatch env ann sumExpr matches catchAll = do
  dataType <- checkCompleteness env ann matches catchAll
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
  Environment ->
  Map TyCon TypeConstructor ->
  TyCon ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
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

applyList :: [MonoType] -> MonoType -> TcMonad (Substitutions, MonoType)
applyList vars tyFun = case vars of
  [] -> pure (mempty, tyFun)
  (var : vars') -> do
    tyRes <- getUnknown mempty
    s1 <-
      unify
        (MTFunction mempty var tyRes)
        tyFun
    (s2, tyFun') <- applyList vars' (applySubst s1 tyRes)
    pure (s2 <> s1, applySubst (s2 <> s1) tyFun')

inferOperator ::
  Environment ->
  Annotation ->
  Operator ->
  Expr Variable Annotation ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
inferOperator env ann Equals a b = do
  (s1, tyA) <- infer env a
  (s2, tyB) <- infer env b
  case tyA of
    MTFunction {} -> throwError $ NoFunctionEquality tyA tyB
    _ -> do
      s3 <- unify tyA tyB -- Equals wants them to be the same
      pure (s3 <> s2 <> s1, MTPrim ann MTBool)
inferOperator env ann Add a b = inferNumericInfix env ann a b
inferOperator env ann Subtract a b = inferNumericInfix env ann a b

inferNumericInfix ::
  Environment ->
  Annotation ->
  Expr Variable Annotation ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
inferNumericInfix env ann a b = do
  (s1, tyA) <- infer env a
  (s2, tyB) <- infer env b
  s3 <- unify tyB (MTPrim mempty MTInt)
  s4 <- unify tyA (MTPrim mempty MTInt)
  pure (s4 <> s3 <> s2 <> s1, MTPrim ann MTInt)

inferRecordAccess ::
  Environment ->
  Annotation ->
  Expr Variable Annotation ->
  Name ->
  TcMonad (Substitutions, MonoType)
inferRecordAccess env ann a name = do
  (s1, tyItems) <- infer env a
  tyResult <- case tyItems of
    (MTRecord _ bits) ->
      case M.lookup name bits of
        Just mt -> pure mt
        _ ->
          throwError $ MissingRecordTypeMember ann name bits
    (MTVar ann' _) -> getUnknown ann'
    _ -> throwError $ CannotMatchRecord env ann tyItems
  s2 <-
    unify
      (MTRecord mempty $ M.singleton name tyResult)
      tyItems
  let subs = s2 <> s1
  pure (subs, applySubst subs tyResult)

infer ::
  Environment ->
  Expr Variable Annotation ->
  TcMonad (Substitutions, MonoType)
infer env inferExpr =
  case inferExpr of
    (MyLiteral ann a) -> inferLiteral ann a
    (MyVar ann name) ->
      inferVarFromScope env ann name
    (MyRecord ann map') -> do
      tyRecord <- getUnknown ann
      (s1, tyResult) <- splitRecordTypes <$> traverse (infer env) map'
      s2 <- unify tyResult tyRecord
      pure
        ( s2 <> s1,
          applySubst (s2 <> s1) tyRecord
        )
    (MyInfix ann op a b) -> inferOperator env ann op a b
    (MyLet ann binder expr body) ->
      inferLetBinding env ann binder expr body
    (MyRecordAccess ann (MyRecord _ items') name) ->
      case M.lookup name items' of
        Just item ->
          infer env item
        Nothing ->
          throwError $ MissingRecordMember ann name (S.fromList (M.keys items'))
    (MyRecordAccess ann a name) ->
      inferRecordAccess env ann a name
    (MyLetPair _ binder1 binder2 expr body) ->
      inferLetPairBinding env binder1 binder2 expr body
    (MyLambda ann binder body) -> do
      tyBinder <- getUnknown ann
      let tmpCtx =
            createEnv binder (Scheme [] tyBinder)
              <> env
      (s1, tyBody) <- infer tmpCtx body
      pure (s1, MTFunction ann (applySubst s1 tyBinder) tyBody)
    (MyApp ann function argument) -> inferApplication env ann function argument
    (MyIf _ condition thenCase elseCase) -> do
      (s1, tyCond) <- infer env condition
      (s2, tyThen) <- infer (applySubstCtx s1 env) thenCase
      (s3, tyElse) <- infer (applySubstCtx s1 env) elseCase
      s4 <- unify tyThen tyElse
      s5 <- unify tyCond (MTPrim (getAnnotation condition) MTBool)
      let subs = s5 <> s4 <> s3 <> s2 <> s1
      pure
        ( subs,
          applySubst subs tyElse
        )
    (MyPair ann a b) -> do
      (s1, tyA) <- infer env a
      (s2, tyB) <- infer env b
      let subs = s2 <> s1
      pure (subs, MTPair ann tyA tyB)
    (MyData _ dataType expr) ->
      storeDataDeclaration env dataType expr
    (MyConstructor ann name) ->
      inferDataConstructor env ann name
    (MyConsApp ann cons val) ->
      inferApplication env ann cons val
    (MyCaseMatch ann expr' matches catchAll) ->
      inferCaseMatch env ann expr' matches catchAll
