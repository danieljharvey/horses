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
import Data.Map (Map)
import qualified Data.Map as M
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
  Swaps ->
  Expr Variable ->
  Either TypeError MonoType
startInference swaps expr = snd <$> doInference swaps mempty expr

doInference ::
  Swaps ->
  Environment ->
  Expr Variable ->
  Either TypeError (Substitutions, MonoType)
doInference swaps env expr = runTcMonad swaps (infer (defaultEnv <> env) expr)

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

-- given a datatype declaration, checks it makes sense and if so,
-- add it to the Environment
inferDataDeclaration ::
  Environment ->
  Construct ->
  [Name] ->
  Map Construct [TypeName] ->
  Expr Variable ->
  TcMonad (Substitutions, MonoType)
inferDataDeclaration env tyName tyArgs constructors' expr' =
  if M.member tyName (getDataTypes env)
    then throwError (DuplicateTypeDeclaration tyName)
    else
      let newEnv = Environment mempty (M.singleton tyName (tyArgs, constructors'))
       in infer (newEnv <> env) expr'

-- infer the type of a data constructor
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferDataConstructor :: Environment -> Construct -> TcMonad (Substitutions, MonoType)
inferDataConstructor env name = do
  (ty, (tyVarNames, constructors)) <- lookupConstructor env name
  args <- findConstructorArgs constructors name
  (tyVars, tyArgs) <- inferArgTypes env ty tyVarNames args
  case args of
    [] -> pure (mempty, MTData ty tyVars)
    _ ->
      pure (mempty, foldr MTFunction (MTData ty tyVars) tyArgs)

-- infer types for data type and it's constructor in one big go
inferArgTypes ::
  Environment ->
  Construct ->
  [Name] ->
  [TypeName] ->
  TcMonad ([MonoType], [MonoType])
inferArgTypes env type' tyNames tyArgs = do
  tyVars <- traverse (\a -> (,) <$> pure a <*> getUnknown) tyNames
  let findType ty = case ty of
        ConsName cn vs -> do
          vs' <- traverse findType vs
          inferType env cn vs'
        VarName var ->
          case filter (\(tyName, _) -> tyName == var) tyVars of
            [(_, tyFound)] -> pure tyFound
            _ -> throwError $ TypeVariableNotInDataType type' var (fst <$> tyVars)
  tyCons <- traverse findType tyArgs
  pure (snd <$> tyVars, tyCons)

-- parse a type from it's name
-- this will soon become insufficient for more complex types
inferType :: Environment -> Construct -> [MonoType] -> TcMonad MonoType
inferType env tyName tyVars =
  case M.lookup tyName (getDataTypes env) of
    (Just _) -> case lookupBuiltIn tyName of
      Just mt -> pure mt
      _ -> pure (MTData tyName tyVars)
    _ -> throwError (TypeConstructorNotInScope env tyName)

-- given a data type, return the arguments for a given constructor
findConstructorArgs ::
  Map Construct [TypeName] ->
  Construct ->
  TcMonad [TypeName]
findConstructorArgs type' name =
  let withMatchingConstructor = M.lookup name
   in case withMatchingConstructor type' of
        Just args -> pure args
        _ -> throwError (ConflictingConstructors name)

lookupBuiltIn :: Construct -> Maybe MonoType
lookupBuiltIn name = M.lookup name builtInTypes

-----

-- check a list of types are all the same
matchList :: [MonoType] -> TcMonad (Substitutions, MonoType)
matchList =
  foldl
    ( \ty' tyB' -> do
        (sA, tyA) <- ty'
        sB <- unify tyA tyB'
        pure (sA <> sB, applySubst sB tyB')
    )
    ((,) <$> pure mempty <*> getUnknown)

-----

inferMyCaseMatch ::
  Environment ->
  Expr Variable ->
  [(Construct, Expr Variable)] ->
  Maybe (Expr Variable) ->
  TcMonad (Substitutions, MonoType)
inferMyCaseMatch env sumExpr matches catchAll = do
  checkCompleteness env matches catchAll
  (s1, _) <- infer env sumExpr
  tyMatches <- traverse (uncurry (inferMatch (applySubstCtx s1 env))) matches
  (sCatch, tyCatches) <- case catchAll of
    Just catchAll' -> do
      (s, tyCatchAll) <- infer (applySubstCtx s1 env) catchAll'
      pure (s, [tyCatchAll])
    _ -> pure (mempty, mempty)
  let subs = mconcat (fst <$> tyMatches) <> s1 <> sCatch -- all substitutions
  let actuals = (snd <$> tyMatches) <> tyCatches
  (sMore, mt) <- matchList actuals
  pure (sMore <> subs, applySubst (sMore <> subs) mt)

-- infer the type of a case match function
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferMatch :: Environment -> Construct -> Expr Variable -> TcMonad (Substitutions, MonoType)
inferMatch env name expr' = do
  (ty, (tyVarNames, constructors)) <- lookupConstructor env name
  args <- findConstructorArgs constructors name
  (_, tyArgs) <- inferArgTypes env ty tyVarNames args
  case args of
    [] -> infer env expr' -- no arguments to pass to expr'
    _ -> do
      (s1, tyExpr) <- infer env expr' -- expression return
      (s2, tyRes) <- applyList tyArgs tyExpr
      let subs = s2 <> s1
      pure (subs, applySubst subs tyRes)

-- given a function type and a list of args, apply then one by one
applyList :: [MonoType] -> MonoType -> TcMonad (Substitutions, MonoType)
applyList vars func = case vars of
  [] -> pure (mempty, func)
  (var : vars') -> do
    tyRes <- getUnknown
    s1 <- unify func (MTFunction var tyRes)
    (s2, tyFunc) <- applyList vars' (applySubst s1 tyRes)
    pure (s2 <> s1, applySubst (s2 <> s1) tyFunc)

infer :: Environment -> Expr Variable -> TcMonad (Substitutions, MonoType)
infer env inferExpr =
  case inferExpr of
    (MyLiteral a) -> inferLiteral a
    (MyVar name) ->
      inferVarFromScope env name
        <|> inferBuiltIn name
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
    (MyLetPair binder1 binder2 expr body) ->
      inferLetPairBinding env binder1 binder2 expr body
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
    (MyData tyName tyArgs constructors expr) ->
      inferDataDeclaration env tyName tyArgs constructors expr
    (MyConstructor name) ->
      inferDataConstructor env name
    (MyConsApp cons val) ->
      inferApplication env cons val
    (MyCaseMatch expr' matches catchAll) ->
      inferMyCaseMatch env expr' matches catchAll
