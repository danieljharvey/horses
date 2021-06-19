{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Infer
  ( startInference,
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
import Language.Mimsa.ExprUtils
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.Exhaustiveness
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.Patterns (checkCompleteness)
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.TypedHoles
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type TcExpr = Expr Variable Annotation

startInference ::
  Map Name MonoType ->
  Swaps ->
  TcExpr ->
  Either TypeError MonoType
startInference typeMap swaps expr = snd <$> doInference typeMap swaps mempty expr

doInference ::
  Map Name MonoType ->
  Swaps ->
  Environment ->
  TcExpr ->
  Either TypeError (Substitutions, MonoType)
doInference typeMap swaps env expr = runTcMonad swaps $ do
  (subs, mt) <- inferAndSubst (defaultEnv mempty <> env) expr
  typedHolesCheck typeMap subs mt

doDataTypeInference ::
  Environment ->
  DataType Annotation ->
  Either TypeError (Map TyCon TypeConstructor)
doDataTypeInference env dt =
  runTcMonad mempty (snd <$> inferConstructorTypes (defaultEnv mempty <> env) dt)

-- run inference, and substitute everything possible
inferAndSubst ::
  Environment ->
  TcExpr ->
  TcMonad (Substitutions, MonoType)
inferAndSubst env expr = do
  (s, tyExpr) <- infer env expr
  pure (s, applySubst s tyExpr)

applySubstCtx :: Substitutions -> Environment -> Environment
applySubstCtx subst (Environment schemes dt inf) =
  Environment (M.map (applySubstScheme subst) schemes) dt inf

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

inferVarFromScope ::
  Environment ->
  Annotation ->
  Variable ->
  TcMonad (Substitutions, MonoType)
inferVarFromScope env@(Environment env' _ _) ann var' =
  case M.lookup
    (variableToTypeIdentifier var')
    env' of
    Just mt ->
      instantiate mt
    _ -> do
      swaps <- ask
      throwError $
        VariableNotInEnv
          swaps
          ann
          var'
          (S.fromList (M.keys (getSchemes env)))

envFromVar :: Variable -> Scheme -> Environment
envFromVar binder scheme =
  Environment (M.singleton (variableToTypeIdentifier binder) scheme) mempty mempty

envFromInfixOp :: InfixOp -> MonoType -> Environment
envFromInfixOp infixOp mt =
  Environment
    mempty
    mempty
    (M.singleton infixOp mt)

lookupInfixOp :: Environment -> Annotation -> InfixOp -> TcMonad MonoType
lookupInfixOp env ann infixOp = do
  case M.lookup infixOp (getInfix env) of
    Just mt' -> pure mt'
    Nothing ->
      throwError
        ( CouldNotFindInfixOperator
            ann
            infixOp
            (M.keysSet (getInfix env))
        )

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
  TcExpr ->
  TcExpr ->
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
  TcExpr ->
  TcExpr ->
  TcMonad (Substitutions, MonoType)
inferLetBinding env ann binder expr body = do
  tyUnknown <- getUnknown ann
  binderInExpr <- findActualBindingInSwaps binder
  let newEnv1 = envFromVar binderInExpr (Scheme mempty tyUnknown) <> env
  (s1, tyExpr) <- infer newEnv1 expr
  let newEnv2 = envFromVar binder (generalise s1 tyExpr) <> newEnv1
  (s2, tyBody) <- infer (applySubstCtx s1 newEnv2) body
  s3 <- unify tyUnknown tyExpr
  pure (s3 <> s2 <> s1, applySubst s3 tyBody)

inferLetPairBinding ::
  Environment ->
  Variable ->
  Variable ->
  TcExpr ->
  TcExpr ->
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
      newEnv = envFromVar binder1 schemeA <> envFromVar binder2 schemeB <> env
  s2 <- unify tyExpr (MTPair mempty tyA tyB)
  (s3, tyBody) <- infer (applySubstCtx (s2 <> s1) newEnv) body
  pure (s3 <> s2 <> s1, tyBody)

inferIf :: Environment -> TcExpr -> TcExpr -> TcExpr -> TcMonad (Substitutions, MonoType)
inferIf env condition thenExpr elseExpr = do
  (s1, tyCond) <- infer env condition
  (s2, tyThen) <- infer (applySubstCtx s1 env) thenExpr
  (s3, tyElse) <- infer (applySubstCtx s1 env) elseExpr
  s4 <- unify tyThen tyElse
  s5 <- unify tyCond (MTPrim (getAnnotation condition) MTBool)
  let subs = s5 <> s4 <> s3 <> s2 <> s1
  pure
    ( subs,
      applySubst subs tyElse
    )

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
  TcExpr ->
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
  TcExpr ->
  NonEmpty (TyCon, TcExpr) ->
  Maybe TcExpr ->
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
  (s, mt) <- matchList (applySubst matchSubs <$> actuals)
  let allSubs = s2 <> s1 <> s <> matchSubs
  pure (allSubs, applySubst allSubs mt)

applySubstToConstructor :: Substitutions -> TypeConstructor -> TypeConstructor
applySubstToConstructor subs (TypeConstructor name ty b) =
  TypeConstructor name (applySubst subs <$> ty) (applySubst subs <$> b)

getA :: (a, b, c) -> a
getA (a, _, _) = a

getB :: (a, b, c) -> b
getB (_, b, _) = b

getC :: (a, b, c) -> c
getC (_, _, c) = c

-- check type of input expr
-- check input against patterns
-- check patterns are complete
-- check output types are the same
inferPatternMatch ::
  Environment ->
  Annotation ->
  TcExpr ->
  [(Pattern Variable Annotation, TcExpr)] ->
  TcMonad (Substitutions, MonoType)
inferPatternMatch env ann expr patterns = do
  -- ensure we even have any patterns to match on
  _ <- checkEmptyPatterns ann patterns
  (s1, tyExpr) <- infer env expr
  -- infer types of all patterns
  tyPatterns <-
    traverse
      ( \(pat, patternExpr) -> do
          (ps1, tyPattern, newEnv) <- inferPattern (applySubstCtx s1 env) pat
          ps2 <- unify tyPattern tyExpr
          (ps3, tyPatternExpr) <- infer (applySubstCtx (ps2 <> ps1) newEnv) patternExpr
          let pSubs = ps3 <> ps2 <> ps1
          pure (pSubs, applySubst pSubs tyPattern, tyPatternExpr)
      )
      patterns
  -- combine all subs we've created in the above
  let subs = mconcat (getA <$> tyPatterns)
  -- combine all patterns to check their types match
  (s2, tyMatchedPattern) <- matchList (getB <$> tyPatterns)
  -- match patterns with match expr
  s3 <- unify tyMatchedPattern (applySubst (s2 <> subs) tyExpr)
  -- combine all output expr types
  (s4, tyMatchedExprs) <- matchList (applySubst s3 <$> (getC <$> tyPatterns))
  -- get all the subs we've learned about
  let allSubs = s4 <> s3 <> s2 <> subs <> s1
  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann (fst <$> patterns)
  pure (allSubs, applySubst allSubs tyMatchedExprs)

checkEmptyPatterns :: Annotation -> [a] -> TcMonad (NE.NonEmpty a)
checkEmptyPatterns ann as = case as of
  [] -> throwError (PatternMatchErr $ EmptyPatternMatch ann)
  other -> pure (NE.fromList other)

inferPattern ::
  Environment ->
  Pattern Variable Annotation ->
  TcMonad (Substitutions, MonoType, Environment)
inferPattern env (PLit ann lit) = do
  (s, mt) <- infer env (MyLiteral ann lit)
  pure (s, mt, env)
inferPattern env (PVar ann binder) = do
  tyBinder <- getUnknown ann
  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env
  pure (mempty, tyBinder, tmpCtx)
inferPattern env (PWildcard ann) = do
  tyUnknown <- getUnknown ann
  pure (mempty, tyUnknown, env)
inferPattern env (PConstructor ann tyCon args) = do
  tyEverything <- traverse (inferPattern env) args
  let allSubs = mconcat (getA <$> tyEverything)
  let tyArgs = getB <$> tyEverything
  let newEnv = mconcat (getC <$> tyEverything) <> env
  dt@(DataType ty _ _) <- lookupConstructor newEnv ann tyCon
  -- we get the types for the constructor in question
  -- and unify them with the tests in the pattern
  consType <- inferConstructorTypes env dt
  (s, tyTypeVars) <- case M.lookup tyCon (snd consType) of
    Just (TypeConstructor _ dtTypeVars tyDtArgs) -> do
      let tyPairs = zip tyArgs tyDtArgs
      subs <- traverse (uncurry unify) tyPairs
      let tySubs = mconcat subs <> allSubs
      pure (tySubs, applySubst tySubs <$> dtTypeVars)
    _ -> throwError UnknownTypeError
  checkArgsLength ann dt tyCon tyArgs
  pure (s <> allSubs, MTData ann ty (applySubst (s <> allSubs) <$> tyTypeVars), applySubstCtx (s <> allSubs) newEnv)
inferPattern env (PPair ann a b) = do
  (s1, tyA, envA) <- inferPattern env a
  (s2, tyB, envB) <- inferPattern envA b
  pure (s2 <> s1, applySubst (s2 <> s1) (MTPair ann tyA tyB), envB)
inferPattern env (PRecord ann items) = do
  let inferRow (k, v) = do
        (s, tyValue, envNew) <- inferPattern env v
        pure (s, M.singleton k tyValue, envNew)
  tyEverything <- traverse inferRow (M.toList items)
  let allSubs = mconcat (getA <$> tyEverything)
  let tyItems = mconcat (getB <$> tyEverything)
  let newEnv = mconcat (getC <$> tyEverything) <> env
  tyRest <- getUnknown ann
  pure
    ( allSubs,
      applySubst allSubs (MTRecordRow ann tyItems tyRest),
      newEnv
    )
inferPattern env (PArray ann items spread) = do
  let inferRow v = do
        (s, tyValue, envNew) <- inferPattern env v
        pure (s, tyValue, envNew)
  tyEverything <- traverse inferRow items
  let allSubs = mconcat (getA <$> tyEverything)
  (s1, tyBinder, env2) <- case spread of
    SpreadValue ann2 binder -> do
      tyBinder <- getUnknown ann2
      let tmpCtx =
            envFromVar binder (Scheme [] (MTArray ann2 tyBinder)) <> env
      pure (mempty, Just tyBinder, tmpCtx)
    _ -> pure (mempty, Nothing, env)
  (s2, tyItems) <- matchList ((getB <$> tyEverything) <> maybe mempty pure tyBinder)
  let newEnv = mconcat (getC <$> tyEverything) <> env2
  pure
    ( s2 <> s1 <> allSubs,
      applySubst (s2 <> s1 <> allSubs) (MTArray ann tyItems),
      newEnv
    )

checkArgsLength :: Annotation -> DataType ann -> TyCon -> [a] -> TcMonad ()
checkArgsLength ann (DataType _ _ cons) tyCon args = do
  case M.lookup tyCon cons of
    Just consArgs ->
      if length consArgs == length args
        then pure ()
        else
          throwError $
            PatternMatchErr
              ( ConstructorArgumentLengthMismatch
                  ann
                  tyCon
                  (length consArgs)
                  (length args)
              )
    Nothing -> throwError UnknownTypeError -- shouldn't happen (but will)

-- infer the type of a case match function
-- if it has no args, it's a simple MTData
-- however if it has args it becomes a MTFun from args to the MTData
inferMatch ::
  Environment ->
  Map TyCon TypeConstructor ->
  TyCon ->
  TcExpr ->
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
  TcExpr ->
  TcExpr ->
  TcMonad (Substitutions, MonoType)
inferOperator env ann Equals a b = do
  (s1, tyA) <- infer env a
  (s2, tyB) <- infer env b
  case tyA of
    MTFunction {} -> throwError $ NoFunctionEquality tyA tyB
    _ -> do
      s3 <- unify tyA tyB -- Equals wants them to be the same
      pure (s3 <> s2 <> s1, MTPrim ann MTBool)
inferOperator env ann Add a b =
  inferInfix env (MTPrim ann MTInt) a b
inferOperator env ann Subtract a b =
  inferInfix env (MTPrim ann MTInt) a b
inferOperator env ann StringConcat a b =
  inferInfix env (MTPrim ann MTString) a b
inferOperator env ann ArrayConcat a b =
  inferInfix env (MTArray ann (MTVar mempty (TVName "a"))) a b
inferOperator env ann (Custom infixOp) a b = do
  tyRes <- getUnknown ann
  tyFun <- lookupInfixOp env ann infixOp
  (s1, tyArgA) <- infer env a
  (s2, tyArgB) <- infer (applySubstCtx s1 env) b
  s3 <-
    unify
      (applySubst s2 tyFun)
      (MTFunction ann tyArgA (MTFunction ann tyArgB tyRes))
  pure (s3, tyRes)

inferInfix ::
  Environment ->
  MonoType ->
  TcExpr ->
  TcExpr ->
  TcMonad (Substitutions, MonoType)
inferInfix env mt a b = do
  (s1, tyA) <- infer env a
  (s2, tyB) <- infer env b
  s3 <- unify tyB mt
  s4 <- unify tyA mt
  s5 <- unify tyA tyB -- types match, useful for Array
  pure (s5 <> s4 <> s3 <> s2 <> s1, mt)

inferRecordAccess ::
  Environment ->
  Annotation ->
  TcExpr ->
  Name ->
  TcMonad (Substitutions, MonoType)
inferRecordAccess env ann a name = do
  (s1, tyItems) <- infer env a
  let inferRow = \case
        (MTRecord _ bits) ->
          case M.lookup name bits of
            Just mt -> pure (mempty, mt)
            _ ->
              throwError $ MissingRecordTypeMember ann name bits
        (MTRecordRow _ as rest) ->
          case M.lookup name as of
            Just mt -> pure (mempty, mt)
            _ -> inferRow rest
        (MTVar ann' _) -> do
          tyRest <- getUnknown ann'
          tyItem <- getUnknown ann'
          s <-
            unify
              tyItems
              ( MTRecordRow
                  ann'
                  (M.singleton name (applySubst s1 tyItem))
                  (applySubst s1 tyRest)
              )
          pure (s, applySubst (s1 <> s) tyItem)
        _ -> throwError $ CannotMatchRecord env ann tyItems
  (s2, tyResult) <- inferRow tyItems
  pure (s2 <> s1, applySubst (s2 <> s1) tyResult)

inferLambda ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  TcMonad (Substitutions, MonoType)
inferLambda env@(Environment env' _ _) ann binder body = do
  tyBinder <- case M.lookup (variableToTypeIdentifier binder) env' of
    Just (Scheme _ found) -> pure found
    _ -> getUnknown ann
  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env
  (s1, tyBody) <- infer tmpCtx body
  pure (s1, MTFunction ann (applySubst s1 tyBinder) tyBody)

isTwoArityFunction :: MonoType -> Bool
isTwoArityFunction (MTFunction _ _ MTFunction {}) = True
isTwoArityFunction _ = False

inferDefineInfix ::
  Environment ->
  Annotation ->
  InfixOp ->
  Variable ->
  TcExpr ->
  TcMonad (Substitutions, MonoType)
inferDefineInfix env ann infixOp bindName expr = do
  u1 <- getUnknown ann
  u2 <- getUnknown ann
  u3 <- getUnknown ann
  (_, tyBind) <- inferVarFromScope env ann bindName
  let arityError = FunctionArityMismatch (getAnnotationForType tyBind) 2 tyBind
  _ <-
    unify tyBind (MTFunction mempty u1 (MTFunction mempty u2 u3))
      <|> throwError arityError
  let newEnv = envFromInfixOp infixOp tyBind <> env
  if isTwoArityFunction tyBind
    then infer newEnv expr
    else throwError arityError

inferArray ::
  Environment ->
  Annotation ->
  [TcExpr] ->
  TcMonad (Substitutions, MonoType)
inferArray env ann items = do
  tyItem <- getUnknown ann
  let foldFn = \as' a' -> do
        (s', ty') <- as'
        (sA, tyB) <- infer env a'
        sB <- unify ty' tyB
        pure (sB <> sA <> s', applySubst sB tyB)
  (subs, tyItems) <-
    foldl
      foldFn
      (pure (mempty, tyItem))
      items
  pure (subs, MTArray ann tyItems)

infer ::
  Environment ->
  TcExpr ->
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
    (MyTypedHole ann name) -> do
      tyRecord <- addTypedHole ann name
      pure (mempty, tyRecord)
    (MyLet ann binder expr body) ->
      inferLetBinding env ann binder expr body
    (MyRecordAccess ann (MyRecord ann' items') name) ->
      inferRecordAccess env ann (MyRecord ann' items') name
    (MyRecordAccess ann a name) ->
      inferRecordAccess env ann a name
    (MyLetPair _ binder1 binder2 expr body) ->
      inferLetPairBinding env binder1 binder2 expr body
    (MyLambda ann binder body) ->
      inferLambda env ann binder body
    (MyApp ann function argument) ->
      inferApplication env ann function argument
    (MyIf _ condition thenCase elseCase) ->
      inferIf env condition thenCase elseCase
    (MyPair ann a b) -> do
      (s1, tyA) <- infer env a
      (s2, tyB) <- infer (applySubstCtx s1 env) b
      let subs = s2 <> s1
      pure (subs, MTPair ann tyA tyB)
    (MyData ann dataType expr) -> do
      newEnv <- storeDataDeclaration env ann dataType
      infer newEnv expr
    (MyArray ann items) -> do
      inferArray env ann items
    (MyConstructor ann name) ->
      inferDataConstructor env ann name
    (MyConsApp ann cons val) ->
      inferApplication env ann cons val
    (MyCaseMatch ann expr' matches catchAll) ->
      inferCaseMatch env ann expr' matches catchAll
    (MyDefineInfix ann infixOp bindName expr) ->
      inferDefineInfix env ann infixOp bindName expr
    (MyPatternMatch ann expr patterns) ->
      inferPatternMatch env ann expr patterns
