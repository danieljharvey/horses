{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Infer (inferAndSubst, runInferM, infer) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State, runState)
import Control.Monad.Writer
import Data.Foldable
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
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.TypedHoles
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type InferM = ExceptT TypeError (WriterT [Constraint] (ReaderT Swaps (State TypecheckState)))

runInferM ::
  Swaps ->
  TypecheckState ->
  InferM a ->
  Either TypeError ([Constraint], TypecheckState, a)
runInferM swaps tcState value =
  case either' of
    ((Right a, constraints), newTcState) -> Right (constraints, newTcState, a)
    ((Left e, _), _) -> Left e
  where
    either' =
      runState
        (runReaderT (runWriterT (runExceptT value)) swaps)
        tcState

type TcExpr = Expr Variable Annotation

-- run inference, and substitute everything possible
inferAndSubst ::
  Map Name MonoType ->
  Swaps ->
  Environment ->
  TcExpr ->
  Either TypeError (Substitutions, [Constraint], MonoType)
inferAndSubst typeMap swaps env expr = do
  let tcAction = do
        (tyExpr, constraints) <- listen (infer (envWithBuiltInTypes <> env) expr)
        subs <- solve constraints
        tyExpr' <- typedHolesCheck typeMap subs tyExpr
        pure (subs, constraints, tyExpr')
  (_, _, (subs, constraints, tyExpr)) <- runInferM swaps defaultTcState tcAction
  pure (subs, constraints, normaliseType $ applySubst subs tyExpr)

instantiate ::
  Scheme ->
  InferM MonoType
instantiate (Scheme vars ty) = do
  newVars <- traverse (const $ getUnknown mempty) vars
  let pairs = zip vars newVars
  let subst = Substitutions $ M.fromList pairs
  pure (applySubst subst ty)

--------------

inferLiteral :: Annotation -> Literal -> InferM MonoType
inferLiteral ann (MyInt _) =
  pure (MTPrim ann MTInt)
inferLiteral ann (MyBool _) =
  pure (MTPrim ann MTBool)
inferLiteral ann (MyString _) =
  pure (MTPrim ann MTString)

inferVarFromScope ::
  Environment ->
  Annotation ->
  Variable ->
  InferM MonoType
inferVarFromScope env@(Environment env' _ _) ann var' =
  case M.lookup (variableToTypeIdentifier var') env' of
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

lookupInfixOp :: Environment -> Annotation -> InfixOp -> InferM MonoType
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

-- let's pattern match on exactly what's inside more clearly
inferApplication ::
  Environment ->
  Annotation ->
  TcExpr ->
  TcExpr ->
  InferM MonoType
inferApplication env ann function argument = do
  tyRes <- getUnknown ann
  tyFun <- infer env function
  tyArg <- infer env argument
  tell
    [ ShouldEqual tyFun (MTFunction ann tyArg tyRes)
    ]
  pure tyRes

-- when we come to do let recursive the name of our binder
-- may already be turned into a number in the expr
-- so we look it up to make sure we bind the right thing
findActualBindingInSwaps :: Variable -> InferM Variable
findActualBindingInSwaps (NamedVar var) = do
  swaps <- ask
  case listToMaybe $ M.keys $ M.filter (== var) swaps of
    Just i -> pure i
    _ -> pure (NamedVar var)
findActualBindingInSwaps a = pure a

bindingIsRecursive :: Variable -> TcExpr -> Bool
bindingIsRecursive var = getAny . withMonoid findBinding
  where
    findBinding (MyVar _ binding) | binding == var = (False, Any True)
    findBinding _ = (True, mempty)

-- if type is recursive we make it monomorphic
-- if not, we make it polymorphic
inferLetBinding ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  TcExpr ->
  InferM MonoType
inferLetBinding env ann binder expr body = do
  if bindingIsRecursive binder expr
    then inferRecursiveLetBinding env ann binder expr body
    else do
      -- we have to run substitutions on this before "saving" it
      (tyExpr, constraints) <- listen (infer env expr)
      subst <- solve constraints
      let tySubstExpr = applySubst subst tyExpr
      let newEnv =
            envFromVar binder (generalise env tySubstExpr)
              <> env
      infer newEnv body

inferRecursiveLetBinding ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  TcExpr ->
  InferM MonoType
inferRecursiveLetBinding env ann binder expr body = do
  binderInExpr <- findActualBindingInSwaps binder
  tyExprAsFix <- infer env (MyLambda mempty binderInExpr expr)
  tyRec <- getUnknown ann
  tyExpr <- getUnknown ann
  tell
    [ ShouldEqual tyExprAsFix (MTFunction mempty tyRec tyRec),
      ShouldEqual tyExprAsFix (MTFunction ann tyRec tyExpr)
    ]
  let newEnv2 =
        envFromVar binder (Scheme [] tyRec)
          <> env
  infer newEnv2 body

inferIf :: Environment -> TcExpr -> TcExpr -> TcExpr -> InferM MonoType
inferIf env condition thenExpr elseExpr = do
  tyCond <- infer env condition
  tyThen <- infer env thenExpr
  tyElse <- infer env elseExpr
  tell
    [ ShouldEqual tyThen tyElse,
      ShouldEqual tyCond (MTPrim (getAnnotation condition) MTBool)
    ]
  pure tyThen

-----

-- check a list of types are all the same
matchList :: NE.NonEmpty MonoType -> InferM MonoType
matchList mts = do
  foldl
    ( \ty' tyB' -> do
        tyA <- ty'
        tell [ShouldEqual tyA tyB']
        pure tyB'
    )
    ( pure (NE.head mts)
    )
    (NE.tail mts)

-- check type of input expr
-- check input against patterns
-- check patterns are complete
-- check output types are the same
inferPatternMatch ::
  Environment ->
  Annotation ->
  TcExpr ->
  [(Pattern Variable Annotation, TcExpr)] ->
  InferM MonoType
inferPatternMatch env ann expr patterns = do
  -- ensure we even have any patterns to match on
  nePatterns <- checkEmptyPatterns ann patterns
  tyExpr <- infer env expr
  -- infer types of all patterns
  tyPatterns <-
    traverse
      ( \(pat, patternExpr) -> do
          (tyPattern, newEnv) <- inferPattern env pat
          tell [ShouldEqual tyPattern tyExpr]
          tyPatternExpr <- infer newEnv patternExpr
          pure (tyPattern, tyPatternExpr)
      )
      nePatterns
  -- combine all patterns to check their types match
  tyMatchedPattern <- matchList (fst <$> tyPatterns)
  -- match patterns with match expr
  tell [ShouldEqual tyMatchedPattern tyExpr]
  -- combine all output expr types
  tyMatchedExprs <- matchList (snd <$> tyPatterns)
  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann (fst <$> patterns)
  pure tyMatchedExprs

-- get non-empty list from list and error if not
checkEmptyPatterns :: Annotation -> [a] -> InferM (NE.NonEmpty a)
checkEmptyPatterns ann as = case as of
  [] -> throwError (PatternMatchErr $ EmptyPatternMatch ann)
  other -> pure (NE.fromList other)

inferPattern ::
  Environment ->
  Pattern Variable Annotation ->
  InferM (MonoType, Environment)
inferPattern env (PLit ann lit) = do
  mt <- infer env (MyLiteral ann lit)
  pure (mt, env)
inferPattern env (PVar ann binder) = do
  tyBinder <- getUnknown ann
  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env
  pure (tyBinder, tmpCtx)
inferPattern env (PWildcard ann) = do
  tyUnknown <- getUnknown ann
  pure (tyUnknown, env)
inferPattern env (PConstructor ann tyCon args) = do
  tyEverything <- traverse (inferPattern env) args
  let tyArgs = fst <$> tyEverything
  let newEnv = mconcat (snd <$> tyEverything) <> env
  dt@(DataType ty _ _) <- lookupConstructor newEnv ann tyCon
  -- we get the types for the constructor in question
  -- and unify them with the tests in the pattern
  consType <- inferConstructorTypes env dt
  tyTypeVars <- case M.lookup tyCon (snd consType) of
    Just (TypeConstructor _ dtTypeVars tyDtArgs) -> do
      let tyPairs = zip tyArgs tyDtArgs
      traverse_ (\(a, b) -> tell [ShouldEqual a b]) tyPairs
      pure dtTypeVars
    _ -> throwError UnknownTypeError
  checkArgsLength ann dt tyCon tyArgs
  pure
    ( MTData ann ty tyTypeVars,
      newEnv
    )
inferPattern env (PPair ann a b) = do
  (tyA, envA) <- inferPattern env a
  (tyB, envB) <- inferPattern envA b
  pure (MTPair ann tyA tyB, envB)
inferPattern env (PRecord ann items) = do
  let inferRow (k, v) = do
        (tyValue, envNew) <- inferPattern env v
        pure (M.singleton k tyValue, envNew)
  tyEverything <- traverse inferRow (M.toList items)
  let tyItems = mconcat (fst <$> tyEverything)
  let newEnv = mconcat (snd <$> tyEverything) <> env
  tyRest <- getUnknown ann
  pure
    ( MTRecordRow ann tyItems tyRest,
      newEnv
    )
inferPattern env (PArray ann items spread) = do
  tyEverything <- traverse (inferPattern env) items
  (tyBinder, env2) <- case spread of
    SpreadValue ann2 binder -> do
      tyBinder <- getUnknown ann2
      let tmpCtx =
            envFromVar binder (Scheme [] (MTArray ann2 tyBinder)) <> env
      pure (Just tyBinder, tmpCtx)
    _ -> pure (Nothing, env)
  tyItems <- case NE.nonEmpty ((fst <$> tyEverything) <> maybe mempty pure tyBinder) of
    Just neItems -> matchList neItems
    _ -> getUnknown ann
  let newEnv = mconcat (snd <$> tyEverything) <> env2
  pure
    ( MTArray ann tyItems,
      newEnv
    )
inferPattern env (PString ann a as) = do
  let envFromStrPart x = case x of
        (StrValue ann' name) ->
          envFromVar name (Scheme [] (MTPrim ann' MTString))
        _ -> mempty
  let newEnv = envFromStrPart a <> envFromStrPart as <> env
  pure (MTPrim ann MTString, newEnv)

checkArgsLength :: Annotation -> DataType -> TyCon -> [a] -> InferM ()
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

inferOperator ::
  Environment ->
  Annotation ->
  Operator ->
  TcExpr ->
  TcExpr ->
  InferM MonoType
inferOperator env ann Equals a b = do
  tyA <- infer env a
  tyB <- infer env b
  case tyA of
    MTFunction {} -> throwError $ NoFunctionEquality tyA tyB
    _ -> do
      tell [ShouldEqual tyA tyB] -- Equals wants them to be the same
      pure (MTPrim ann MTBool)
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
  tyArgA <- infer env a
  tyArgB <- infer env b
  tell
    [ ShouldEqual
        tyFun
        (MTFunction ann tyArgA (MTFunction ann tyArgB tyRes))
    ]
  pure tyRes

inferInfix ::
  Environment ->
  MonoType ->
  TcExpr ->
  TcExpr ->
  InferM MonoType
inferInfix env mt a b = do
  tyA <- infer env a
  tyB <- infer env b
  tell [ShouldEqual tyA tyB, ShouldEqual tyB mt, ShouldEqual tyA mt]
  pure mt

inferRecordAccess ::
  Environment ->
  Annotation ->
  TcExpr ->
  Name ->
  InferM MonoType
inferRecordAccess env ann a name = do
  tyItems <- infer env a
  let inferRow = \case
        (MTRecord _ bits) ->
          case M.lookup name bits of
            Just mt -> pure mt
            _ ->
              throwError $ MissingRecordTypeMember ann name bits
        (MTRecordRow _ as rest) ->
          case M.lookup name as of
            Just mt -> pure mt
            _ -> inferRow rest
        (MTVar ann' _) -> do
          tyRest <- getUnknown ann'
          tyItem <- getUnknown ann'
          tell
            [ ShouldEqual
                tyItems
                ( MTRecordRow
                    ann'
                    (M.singleton name tyItem)
                    tyRest
                )
            ]
          pure tyItem
        _ -> throwError $ CannotMatchRecord env ann tyItems
  inferRow tyItems

inferLetPattern ::
  Environment ->
  Annotation ->
  Pattern Variable Annotation ->
  TcExpr ->
  TcExpr ->
  InferM MonoType
inferLetPattern env ann pat expr body = do
  (tyPattern, newEnv) <- inferPattern env pat
  tyExpr <- infer env expr
  tyBody <- infer newEnv body
  tell
    [ ShouldEqual tyPattern tyExpr
    ]

  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann [pat]
  pure tyBody

inferLambda ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  InferM MonoType
inferLambda env ann binder body = do
  tyBinder <- getUnknown ann
  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env
  tyBody <- infer tmpCtx body
  pure (MTFunction ann tyBinder tyBody)

isTwoArityFunction :: MonoType -> Bool
isTwoArityFunction (MTFunction _ _ MTFunction {}) = True
isTwoArityFunction _ = False

inferDefineInfix ::
  Environment ->
  Annotation ->
  InfixOp ->
  Variable ->
  TcExpr ->
  InferM MonoType
inferDefineInfix env ann infixOp bindName expr = do
  u1 <- getUnknown ann
  u2 <- getUnknown ann
  u3 <- getUnknown ann
  tyBind <- inferVarFromScope env ann bindName
  let arityError = FunctionArityMismatch (getAnnotationForType tyBind) 2 tyBind
  tell [ShouldEqual tyBind (MTFunction mempty u1 (MTFunction mempty u2 u3))] -- TODO: should this be InstanceOf
  let newEnv = envFromInfixOp infixOp tyBind <> env
  if isTwoArityFunction tyBind
    then infer newEnv expr
    else throwError arityError

inferArray ::
  Environment ->
  Annotation ->
  [TcExpr] ->
  InferM MonoType
inferArray env ann items = do
  tyItem <- getUnknown ann
  let foldFn = \as' a' -> do
        ty' <- as'
        tyB <- infer env a'
        tell [ShouldEqual ty' tyB]
        pure tyB
  tyItems <-
    foldl
      foldFn
      (pure tyItem)
      items
  pure (MTArray ann tyItems)

infer ::
  Environment ->
  TcExpr ->
  InferM MonoType
infer env inferExpr =
  case inferExpr of
    (MyLiteral ann a) -> inferLiteral ann a
    (MyVar ann name) ->
      inferVarFromScope env ann name
    (MyRecord ann map') ->
      MTRecord ann <$> traverse (infer env) map'
    (MyInfix ann op a b) -> inferOperator env ann op a b
    (MyTypedHole ann name) ->
      addTypedHole ann name
    (MyLet ann binder expr body) ->
      inferLetBinding env ann binder expr body
    (MyLetPattern ann pat expr body) ->
      inferLetPattern env ann pat expr body
    (MyRecordAccess ann (MyRecord ann' items') name) ->
      inferRecordAccess env ann (MyRecord ann' items') name
    (MyRecordAccess ann a name) ->
      inferRecordAccess env ann a name
    (MyLambda ann binder body) ->
      inferLambda env ann binder body
    (MyApp ann function argument) ->
      inferApplication env ann function argument
    (MyIf _ condition thenCase elseCase) ->
      inferIf env condition thenCase elseCase
    (MyPair ann a b) -> do
      tyA <- infer env a
      tyB <- infer env b
      pure (MTPair ann tyA tyB)
    (MyData ann dataType expr) -> do
      newEnv <- storeDataDeclaration env ann dataType
      infer newEnv expr
    (MyArray ann items) -> do
      inferArray env ann items
    (MyConstructor ann name) ->
      inferDataConstructor env ann name
    (MyConsApp ann cons val) ->
      inferApplication env ann cons val
    (MyDefineInfix ann infixOp bindName expr) ->
      inferDefineInfix env ann infixOp bindName expr
    (MyPatternMatch ann expr patterns) ->
      inferPatternMatch env ann expr patterns
