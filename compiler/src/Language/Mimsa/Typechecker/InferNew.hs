{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.InferNew (inferAndSubst) where

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
import Language.Mimsa.Logging
import Language.Mimsa.Typechecker.DataTypesNew
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.Exhaustiveness
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.TypedHoles
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type InferM = ExceptT TypeError (WriterT [Constraint] (ReaderT Swaps (State TypecheckState)))

defaultTcState :: TypecheckState
defaultTcState = TypecheckState 0 mempty

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
  Either TypeError (Substitutions, MonoType)
inferAndSubst typeMap swaps env expr = do
  (constraints, tcState, tyExpr) <-
    runInferM swaps defaultTcState (infer env expr)
  (tcState2, subs) <-
    runSolveM swaps tcState (solve (debugPretty "constraints" constraints))
  (_, _, tyExpr') <-
    runInferM swaps tcState2 (typedHolesCheck typeMap subs tyExpr)
  pure (subs, debugPretty "tyExpr" (applySubst (debugPretty "subs" subs) (debugPretty "original" tyExpr')))

polymorphicVersionOf :: Scheme -> InferM MonoType
polymorphicVersionOf (Scheme [] ty) = pure ty
polymorphicVersionOf scheme = do
  tyVal <- getUnknown mempty
  tell [InstanceOf tyVal scheme]
  pure tyVal

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
      polymorphicVersionOf mt
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
  tell [ShouldEqual tyFun (MTFunction ann tyArg tyRes)]
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

-- to allow recursion we make a type for the let binding in it's own expression
-- we may need to unify tyUnknown and tyExpr if it struggles with complex stuff
inferLetBinding ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  TcExpr ->
  InferM MonoType
inferLetBinding env ann binder expr body = do
  tyUnknown <- getUnknown ann
  tyReturn <- getUnknown ann
  binderInExpr <- findActualBindingInSwaps binder
  let newEnv1 = envFromVar binderInExpr (Scheme mempty tyUnknown) <> env
  tyExpr <- infer newEnv1 expr
  let newEnv2 = envFromVar binder (generaliseNoSubs newEnv1 tyExpr) <> newEnv1
  tyBody <- infer newEnv2 body
  tell [ShouldEqual tyUnknown tyExpr, ShouldEqual tyReturn tyBody]
  pure tyReturn

inferIf :: Environment -> TcExpr -> TcExpr -> TcExpr -> InferM MonoType
inferIf env condition thenExpr elseExpr = do
  tyCond <- infer env condition
  tyThen <- infer env thenExpr
  tyElse <- infer env elseExpr
  tell
    [ ShouldEqual tyThen tyElse,
      ShouldEqual tyCond (MTPrim (getAnnotation condition) MTBool)
    ]
  pure tyElse

-----

-- check a list of types are all the same
matchList :: [MonoType] -> InferM MonoType
matchList =
  foldl
    ( \ty' tyB' -> do
        tyA <- ty'
        tell [ShouldEqual tyA tyB']
        pure tyB'
    )
    ( getUnknown mempty
    )

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
  _ <- checkEmptyPatterns ann patterns
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
      patterns
  -- combine all patterns to check their types match
  tyMatchedPattern <- matchList (fst <$> tyPatterns)
  -- match patterns with match expr
  tell [ShouldEqual tyMatchedPattern tyExpr]
  -- combine all output expr types
  tyMatchedExprs <- matchList (snd <$> tyPatterns)
  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann (fst <$> patterns)
  pure tyMatchedExprs

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
  tyItems <- matchList ((fst <$> tyEverything) <> maybe mempty pure tyBinder)
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

checkArgsLength :: Annotation -> DataType ann -> TyCon -> [a] -> InferM ()
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
  tell [ShouldEqual tyB mt, ShouldEqual tyA mt, ShouldEqual tyA tyB]
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
  tell [ShouldEqual tyPattern tyExpr]
  tyBody <- infer newEnv body
  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann [pat]

  pure tyBody

inferLambda ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  InferM MonoType
inferLambda env@(Environment env' _ _) ann binder body = do
  tyReturn <- getUnknown ann
  tyBinder <- case M.lookup (variableToTypeIdentifier binder) env' of
    Just (Scheme _ found) -> pure found
    _ -> getUnknown ann
  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env
  tyBody <- infer tmpCtx body
  tell [ShouldEqual tyReturn (MTFunction ann tyBinder tyBody)]
  pure tyReturn

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
  tell [ShouldEqual tyBind (MTFunction mempty u1 (MTFunction mempty u2 u3))]

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
    (MyRecord ann map') -> do
      tyRecord <- getUnknown ann
      tyResult <- MTRecord ann <$> traverse (infer env) map'
      tell [ShouldEqual tyResult tyRecord]
      pure tyRecord
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
      tyPair <- getUnknown ann
      tyA <- infer env a
      tyB <- infer env b
      tell [ShouldEqual tyPair (MTPair ann tyA tyB)]
      pure tyPair
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
