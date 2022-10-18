{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Typechecker.Elaborate
  ( elab,
    infer,
    recoverAnn,
    getTypeFromAnn,
  )
where

import Control.Monad.Except
import Control.Monad.State (State)
import Control.Monad.Writer
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Language.Mimsa.ExprUtils
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.Exhaustiveness
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.ScopeTypeVar
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Substitutions

import Language.Mimsa.Types.Typechecker.Unique

type ElabM =
  ExceptT
    TypeError
    ( WriterT
        [Constraint]
        (State TypecheckState)
    )

type TcExpr = Expr (Name, Unique) Annotation

recoverAnn :: MonoType -> Annotation
recoverAnn = getAnnotationForType

getTypeFromAnn :: Expr var MonoType -> MonoType
getTypeFromAnn = getAnnotation

getPatternTypeFromAnn :: Pattern (Name, Unique) MonoType -> MonoType
getPatternTypeFromAnn pat =
  case pat of
    PLit ann _ -> ann
    PWildcard ann -> ann
    PVar ann _ -> ann
    PConstructor ann _ _ _ -> ann
    PPair ann _ _ -> ann
    PRecord ann _ -> ann
    PArray ann _ _ -> ann
    PString ann _ _ -> ann

getSpreadTypeFromAnn :: Spread (Name, Unique) MonoType -> Maybe MonoType
getSpreadTypeFromAnn (SpreadValue ann _) = Just ann
getSpreadTypeFromAnn _ = Nothing

type ElabExpr = Expr (Name, Unique) MonoType

--------------

inferLiteral :: Annotation -> Literal -> ElabM ElabExpr
inferLiteral ann lit =
  let tyLit = case lit of
        (MyInt _) -> MTInt
        (MyBool _) -> MTBool
        (MyString _) -> MTString
   in pure (MyLiteral (MTPrim ann tyLit) lit)

lookupInEnv :: (Name, Unique) -> Environment -> Maybe Scheme
lookupInEnv (name, ModuleDep mHash) env =
  M.lookup mHash (getNamespacedSchemes env) >>= M.lookup name
lookupInEnv (name, unique) env =
  let look v = M.lookup v (getSchemes env)
   in look (variableToTypeIdentifier (name, unique))

inferVarFromScope ::
  Environment ->
  Annotation ->
  (Name, Unique) ->
  ElabM MonoType
inferVarFromScope env ann var' =
  case lookupInEnv var' env of
    Just mt ->
      instantiate ann mt
    _ ->
      throwError $
        VariableNotFound
          ann
          (M.keysSet $ getSchemes env)
          (fst var')

envFromVar :: (Name, Unique) -> Scheme -> Environment
envFromVar binder scheme =
  Environment (M.singleton (variableToTypeIdentifier binder) scheme) mempty mempty mempty mempty

lookupInfixOp ::
  Environment ->
  Annotation ->
  InfixOp ->
  ElabM MonoType
lookupInfixOp env ann infixOp = do
  case M.lookup infixOp (getInfix env) of
    Just scheme -> instantiate ann scheme
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
  ElabM ElabExpr
inferApplication env ann function argument = do
  tyRes <- getUnknown ann
  argument' <- infer env argument

  -- run substitutions on the fn type so we can make better errors
  (elabFunction, constraints) <- listen (infer env function)
  subst <- solve constraints
  let tyFunc = applySubst subst (getTypeFromAnn elabFunction)

  -- check the argument against the function input
  -- to create a better error
  case tyFunc of
    (MTFunction fnAnn mtArg _mtRet) -> do
      _ <-
        unify mtArg (getTypeFromAnn argument')
          `catchError` \_ ->
            throwError
              ( FunctionArgumentMismatch
                  fnAnn
                  mtArg
                  (getTypeFromAnn argument')
              )

      pure ()
    MTVar {} -> pure () -- we still don't know what this is yet, leave it to be worked out in constraint solving
    other ->
      throwError (ApplicationToNonFunction (getAnnotation function) other)

  tell
    [ ShouldEqual
        (getTypeFromAnn elabFunction)
        (MTFunction ann (getTypeFromAnn argument') tyRes)
    ]
  pure (MyApp tyRes elabFunction argument')

bindingIsRecursive :: Identifier (Name, Unique) ann -> TcExpr -> Bool
bindingIsRecursive ident = getAny . withMonoid findBinding
  where
    variable = case ident of
      (Identifier _ a) -> a
    findBinding (MyVar _ _ binding) | binding == variable = (False, Any True)
    findBinding _ = (True, mempty)

binderFromIdentifier :: Identifier var ann -> var
binderFromIdentifier = \case
  (Identifier _ a) -> a

annotationFromIdentifier :: Identifier var ann -> ann
annotationFromIdentifier = \case
  (Identifier ann _) -> ann

-- if type is recursive we make it monomorphic
-- if not, we make it polymorphic
inferLetBinding ::
  Environment ->
  Annotation ->
  Identifier (Name, Unique) Annotation ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
inferLetBinding env ann ident expr body = do
  if bindingIsRecursive ident expr
    then inferRecursiveLetBinding env ann ident expr body
    else do
      let bindAnn = annotationFromIdentifier ident
          bindName = binderFromIdentifier ident
      -- we have to run substitutions on this before "saving" it
      (inferExpr, constraints) <- listen (infer env expr)
      subst <- solve constraints
      let tySubstExpr = applySubst subst (getTypeFromAnn inferExpr)
      let newEnv =
            envFromVar bindName (generalise env tySubstExpr)
              <> env
      inferBody <- infer newEnv body
      pure
        ( MyLet
            (getTypeFromAnn inferBody $> ann) -- we want to make sure we keep the original source location
            (ident $> (tySubstExpr $> bindAnn))
            inferExpr
            inferBody
        )

inferRecursiveLetBinding ::
  Environment ->
  Annotation ->
  Identifier (Name, Unique) Annotation ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
inferRecursiveLetBinding env ann ident expr body = do
  let bindName = binderFromIdentifier ident
      bindAnn = annotationFromIdentifier ident
  tyRecExpr <- getUnknown ann
  let envWithRecursiveFn = envFromVar bindName (Scheme [] tyRecExpr) <> env
  inferExpr <- infer envWithRecursiveFn expr

  let tyExpr = getTypeFromAnn inferExpr
      newEnv =
        envFromVar bindName (Scheme [] tyExpr) <> env

  inferBody <- infer newEnv body
  tell [ShouldEqual tyRecExpr (getTypeFromAnn inferExpr)]

  pure
    ( MyLet
        (getTypeFromAnn inferBody)
        (ident $> (tyExpr $> bindAnn))
        inferExpr
        inferBody
    )

unifyTypeOrError :: MonoType -> MonoType -> TypeError -> ElabM ()
unifyTypeOrError got expected typeErr =
  do
    _ <- unify got expected `catchError` \_ -> throwError typeErr
    pure ()

inferIf :: Environment -> Annotation -> TcExpr -> TcExpr -> TcExpr -> ElabM ElabExpr
inferIf env ann condition thenExpr elseExpr = do
  condExpr <- infer env condition

  -- check condition matches Boolean now as we can raise
  -- a more accurate error, we ignore any substitutions as we'll use the constraint
  -- below to create them later and learn about variables etc
  unifyTypeOrError
    (expAnn condExpr)
    (MTPrim mempty MTBool)
    (IfPredicateIsNotBoolean ann (expAnn condExpr))

  thenExpr' <- infer env thenExpr
  elseExpr' <- infer env elseExpr
  tell
    [ -- check the two clauses have the same reply type
      ShouldEqual
        (getTypeFromAnn thenExpr')
        (getTypeFromAnn elseExpr'),
      -- we still need this constraint to learn about any variables
      -- from the comparison with Boolean
      ShouldEqual
        (getTypeFromAnn condExpr)
        (MTPrim (getAnnotation condition) MTBool)
    ]
  pure
    ( MyIf
        (getTypeFromAnn thenExpr')
        condExpr
        thenExpr'
        elseExpr'
    )

-----

-- check a list of types are all the same
matchList :: NE.NonEmpty MonoType -> ElabM MonoType
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
  [(Pattern (Name, Unique) Annotation, TcExpr)] ->
  ElabM ElabExpr
inferPatternMatch env ann expr patterns = do
  -- ensure we even have any patterns to match on
  nePatterns <- checkEmptyPatterns ann patterns
  -- inferorate source expression that we are matching
  inferExpr <- infer env expr
  -- infer types of all patterns
  inferPatterns <-
    traverse
      ( \(pat, patternExpr) -> do
          (inferPat, newEnv) <- inferPattern env pat
          tell
            [ ShouldEqual
                (getPatternTypeFromAnn inferPat)
                (getTypeFromAnn inferExpr)
            ]
          tyPatternExpr <- infer newEnv patternExpr
          pure (inferPat, tyPatternExpr)
      )
      nePatterns
  -- combine all patterns to check their types match
  tyMatchedPattern <- matchList (getPatternTypeFromAnn . fst <$> inferPatterns)
  -- match patterns with match expr
  tell [ShouldEqual tyMatchedPattern (getTypeFromAnn inferExpr)]
  -- combine all output expr types
  tyMatchedExprs <- matchList (getTypeFromAnn . snd <$> inferPatterns)
  -- remove (,unique) from var
  let patternsWithoutUnique = first fst . fst <$> patterns
  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann patternsWithoutUnique
  -- wrap up the pattern match again
  pure
    ( MyPatternMatch
        tyMatchedExprs
        inferExpr
        (NE.toList inferPatterns)
    )

-- get non-empty list from list and error if not
checkEmptyPatterns :: Annotation -> [a] -> ElabM (NE.NonEmpty a)
checkEmptyPatterns ann as = case as of
  [] -> throwError (PatternMatchErr $ EmptyPatternMatch ann)
  other -> pure (NE.fromList other)

inferPattern ::
  Environment ->
  Pattern (Name, Unique) Annotation ->
  ElabM (Pattern (Name, Unique) MonoType, Environment)
inferPattern env (PLit ann lit) = do
  inferExpr <- infer env (MyLiteral ann lit)
  pure
    ( PLit (getTypeFromAnn inferExpr) lit,
      env
    )
inferPattern env (PVar ann binder) = do
  tyBinder <- getUnknown ann
  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env
  pure
    ( PVar tyBinder binder,
      tmpCtx
    )
inferPattern env (PWildcard ann) = do
  tyUnknown <- getUnknown ann
  pure
    ( PWildcard tyUnknown,
      env
    )
inferPattern env (PConstructor ann modName tyCon args) = do
  inferEverything <- traverse (inferPattern env) args
  let inferArgs = fst <$> inferEverything
  let newEnv = mconcat (snd <$> inferEverything) <> env
  dt@(DataType ty _ _) <- lookupConstructor newEnv ann modName tyCon
  -- we get the types for the constructor in question
  -- and unify them with the tests in the pattern
  consType <- inferConstructorTypes ann modName dt
  tyTypeVars <- case M.lookup tyCon (snd consType) of
    Just (TypeConstructor _ _ dtTypeVars tyDtArgs) -> do
      let tyPairs = zip (getPatternTypeFromAnn <$> inferArgs) tyDtArgs
      traverse_ (\(a, b) -> tell [ShouldEqual a b]) tyPairs
      pure dtTypeVars
    _ -> throwError UnknownTypeError
  checkArgsLength ann dt tyCon inferArgs
  pure
    ( PConstructor (dataTypeWithVars ann modName ty tyTypeVars) modName tyCon inferArgs,
      newEnv
    )
inferPattern env (PPair ann a b) = do
  (inferA, envA) <- inferPattern env a
  (inferB, envB) <- inferPattern envA b
  pure
    ( PPair
        ( MTPair
            ann
            (getPatternTypeFromAnn inferA)
            (getPatternTypeFromAnn inferB)
        )
        inferA
        inferB,
      envB
    )
inferPattern env (PRecord ann items) = do
  let inferRow (k, v) = do
        (tyValue, envNew) <- inferPattern env v
        pure (M.singleton k tyValue, envNew)
  inferEverything <- traverse inferRow (M.toList items)
  let inferItems = mconcat (fst <$> inferEverything)
  let newEnv = mconcat (snd <$> inferEverything) <> env
  tyRest <- getUnknown ann
  pure
    ( PRecord
        ( MTRecord
            ann
            (getPatternTypeFromAnn <$> inferItems)
            (Just tyRest)
        )
        inferItems,
      newEnv
    )
inferPattern env (PArray ann items spread) = do
  inferEverything <- traverse (inferPattern env) items
  (inferSpread, env2) <- case spread of
    SpreadValue ann2 binder -> do
      tyBinder <- getUnknown ann2
      let tmpCtx =
            envFromVar binder (Scheme [] (MTArray ann2 tyBinder)) <> env
      pure
        ( SpreadValue tyBinder binder,
          tmpCtx
        )
    NoSpread -> pure (NoSpread, env)
    SpreadWildcard ann2 -> do
      tyUnknown <- getUnknown ann2
      pure (SpreadWildcard tyUnknown, env)
  tyItems <- case NE.nonEmpty
    ( (getPatternTypeFromAnn . fst <$> inferEverything)
        <> maybe mempty pure (getSpreadTypeFromAnn inferSpread)
    ) of
    Just neItems -> matchList neItems
    _ -> getUnknown ann
  let newEnv = mconcat (snd <$> inferEverything) <> env2
  pure
    ( PArray
        ( MTArray
            ann
            tyItems
        )
        (fst <$> inferEverything)
        inferSpread,
      newEnv
    )
inferPattern env (PString ann a as) = do
  let envFromStrPart x = case x of
        (StrValue ann' name) ->
          envFromVar name (Scheme [] (MTPrim ann' MTString))
        _ -> mempty
  let newEnv = envFromStrPart a <> envFromStrPart as <> env
  let inferStringPart (StrValue ann' name) =
        StrValue (MTPrim ann' MTString) name
      inferStringPart (StrWildcard ann') =
        StrWildcard (MTPrim ann' MTString)
  pure
    ( PString
        (MTPrim ann MTString)
        (inferStringPart a)
        (inferStringPart as),
      newEnv
    )

checkArgsLength :: Annotation -> DataType -> TyCon -> [a] -> ElabM ()
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
  ElabM ElabExpr
inferOperator env ann Equals a b = do
  inferA <- infer env a
  inferB <- infer env b
  let tyA = getTypeFromAnn inferA
      tyB = getTypeFromAnn inferB
  case tyA of
    MTFunction {} -> throwError $ NoFunctionEquality tyA tyB
    _ -> do
      tell
        [ ShouldEqual
            tyA
            tyB -- Equals wants them to be the same
        ]
      pure
        ( MyInfix
            (MTPrim ann MTBool)
            Equals
            inferA
            inferB
        )
inferOperator env ann Add a b = do
  (mt, inferA, inferB) <- inferInfix env (MTPrim ann MTInt) a b
  pure (MyInfix mt Add inferA inferB)
inferOperator env ann Subtract a b = do
  (mt, inferA, inferB) <- inferInfix env (MTPrim ann MTInt) a b
  pure (MyInfix mt Subtract inferA inferB)
inferOperator env ann StringConcat a b = do
  (mt, inferA, inferB) <- inferInfix env (MTPrim ann MTString) a b
  pure (MyInfix mt StringConcat inferA inferB)
inferOperator env ann ArrayConcat a b = do
  tyArr <- getUnknown ann
  (mt, inferA, inferB) <- inferInfix env (MTArray ann tyArr) a b
  pure (MyInfix mt ArrayConcat inferA inferB)
inferOperator env ann GreaterThan a b = do
  (mt, inferA, inferB) <-
    inferComparison
      env
      (MTPrim ann MTInt)
      (MTPrim ann MTBool)
      a
      b
  pure (MyInfix mt GreaterThan inferA inferB)
inferOperator env ann GreaterThanOrEqualTo a b = do
  (mt, inferA, inferB) <-
    inferComparison
      env
      (MTPrim ann MTInt)
      (MTPrim ann MTBool)
      a
      b
  pure (MyInfix mt GreaterThanOrEqualTo inferA inferB)
inferOperator env ann LessThan a b = do
  (mt, inferA, inferB) <-
    inferComparison
      env
      (MTPrim ann MTInt)
      (MTPrim ann MTBool)
      a
      b
  pure (MyInfix mt LessThan inferA inferB)
inferOperator env ann LessThanOrEqualTo a b = do
  (mt, inferA, inferB) <-
    inferComparison
      env
      (MTPrim ann MTInt)
      (MTPrim ann MTBool)
      a
      b
  pure (MyInfix mt LessThanOrEqualTo inferA inferB)
inferOperator env ann (Custom infixOp) a b = do
  tyRes <- getUnknown ann
  tyFun <- lookupInfixOp env ann infixOp
  inferA <- infer env a
  inferB <- infer env b
  tell
    [ ShouldEqual
        tyFun
        ( MTFunction
            ann
            (getTypeFromAnn inferA)
            (MTFunction ann (getTypeFromAnn inferB) tyRes)
        )
    ]
  pure (MyInfix tyRes (Custom infixOp) inferA inferB)

-- | infix operator where inputs and output are the same
inferInfix ::
  Environment ->
  MonoType ->
  TcExpr ->
  TcExpr ->
  ElabM (MonoType, ElabExpr, ElabExpr)
inferInfix env mt a b = do
  inferA <- infer env a
  inferB <- infer env b
  let tyA = getTypeFromAnn inferA
      tyB = getTypeFromAnn inferB
  tell [ShouldEqual tyA tyB, ShouldEqual tyB mt, ShouldEqual tyA mt]
  pure (mt, inferA, inferB)

-- | infix operator where inputs match but output could be different
-- | for instance, 1 < 2 == True would be `Int -> Int -> Bool`
inferComparison ::
  Environment ->
  MonoType ->
  MonoType ->
  TcExpr ->
  TcExpr ->
  ElabM (MonoType, ElabExpr, ElabExpr)
inferComparison env inputMt outputMt a b = do
  inferA <- infer env a
  inferB <- infer env b
  let tyA = getTypeFromAnn inferA
      tyB = getTypeFromAnn inferB
  tell
    [ ShouldEqual tyA tyB,
      ShouldEqual tyA inputMt,
      ShouldEqual tyB inputMt
    ]
  pure (outputMt, inferA, inferB)

inferRecordAccess ::
  Environment ->
  Annotation ->
  TcExpr ->
  Name ->
  ElabM ElabExpr
inferRecordAccess env ann a name = do
  inferItems <- infer env a
  let inferRow = \case
        (MTRecord _ bits Nothing) ->
          case M.lookup name bits of
            Just mt -> pure mt
            _ ->
              throwError $ MissingRecordTypeMember ann name bits
        (MTRecord _ as (Just rest)) ->
          case M.lookup name as of
            Just mt -> pure mt
            _ -> inferRow rest
        (MTVar ann' _) -> do
          tyRest <- getUnknown ann'
          tyItem <- getUnknown ann'
          tell
            [ ShouldEqual
                (getTypeFromAnn inferItems)
                ( MTRecord
                    ann'
                    (M.singleton name tyItem)
                    (Just tyRest)
                )
            ]
          pure tyItem
        _ -> throwError $ CannotMatchRecord env ann (getTypeFromAnn inferItems)
  mt <- inferRow (getTypeFromAnn inferItems)
  pure (MyRecordAccess mt inferItems name)

inferLetPattern ::
  Environment ->
  Annotation ->
  Pattern (Name, Unique) Annotation ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
inferLetPattern env ann pat expr body = do
  inferExpr <- infer env expr
  (inferPat, newEnv) <- inferPattern env pat
  inferBody <- infer newEnv body
  tell
    [ ShouldEqual (getPatternTypeFromAnn inferPat) (getTypeFromAnn inferExpr)
    ]

  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann [first fst pat]

  pure (MyLetPattern (getTypeFromAnn inferBody) inferPat inferExpr inferBody)

inferLambda ::
  Environment ->
  Annotation ->
  Identifier (Name, Unique) Annotation ->
  TcExpr ->
  ElabM ElabExpr
inferLambda env ann ident body = do
  let binder = binderFromIdentifier ident
      bindAnn = annotationFromIdentifier ident

  tyBinder <- getUnknown bindAnn

  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env

  inferBody <- infer tmpCtx body
  let tyReturn = MTFunction ann tyBinder (getTypeFromAnn inferBody)
  pure
    ( MyLambda
        tyReturn
        (ident $> (tyBinder $> bindAnn))
        inferBody
    )

inferArray ::
  Environment ->
  Annotation ->
  [TcExpr] ->
  ElabM ElabExpr
inferArray env ann items = do
  inferItems <- traverse (infer env) items
  tyItems <- case NE.nonEmpty inferItems of
    Just neElabItems -> matchList (getTypeFromAnn <$> neElabItems)
    Nothing -> getUnknown ann
  pure (MyArray (MTArray ann tyItems) inferItems)

elab :: Environment -> TcExpr -> ElabM ElabExpr
elab = infer

checkLambda ::
  Environment ->
  Annotation ->
  Identifier (Name, Unique) Annotation ->
  TcExpr ->
  MonoType ->
  MonoType ->
  ElabM ElabExpr
checkLambda env ann ident body tyBinder tyBody = do
  let binder = binderFromIdentifier ident
      bindAnn = annotationFromIdentifier ident

  -- convert TVName to TVScopedVar and scope them where necessary
  (newEnv1, tyBinder') <- freshNamedType env tyBinder
  (newEnv2, tyBody') <- freshNamedType newEnv1 tyBody

  let envWithBinder =
        envFromVar binder (Scheme [] tyBinder')
          <> newEnv2

  -- check body type
  inferBody <- check envWithBinder body tyBody'

  let tyReturn = MTFunction ann tyBinder' (expAnn inferBody)
  pure
    ( MyLambda
        tyReturn
        (ident $> (tyBinder' $> bindAnn))
        inferBody
    )

check :: Environment -> TcExpr -> MonoType -> ElabM ElabExpr
check env expr mt =
  case (expr, mt) of
    (MyLambda ann ident body, MTFunction _ tyBinder tyBody) ->
      checkLambda env ann ident body tyBinder tyBody
    _ -> do
      typedExpr <- infer env expr
      subs <- unify (expAnn typedExpr) mt
      pure (applySubst subs typedExpr)

infer ::
  Environment ->
  TcExpr ->
  ElabM ElabExpr
infer env inferExpr =
  case inferExpr of
    (MyLiteral ann a) -> inferLiteral ann a
    (MyAnnotation _ann mt expr) -> do
      elabExpr <- check env expr mt
      pure (MyAnnotation (expAnn elabExpr) (mt $> mt) elabExpr)
    (MyVar ann maybeMod name) -> do
      mt <- inferVarFromScope env ann name
      pure (MyVar mt maybeMod name)
    (MyRecord ann map') -> do
      inferItems <- traverse (infer env) map'
      let tyItems = getTypeFromAnn <$> inferItems
      pure (MyRecord (MTRecord ann tyItems Nothing) inferItems)
    (MyInfix ann op a b) -> inferOperator env ann op a b
    (MyTypedHole ann (name, unique)) -> do
      tyHole <- addTypedHole env ann name
      pure (MyVar tyHole Nothing (name, unique))
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
    (MyIf ann condition thenCase elseCase) ->
      inferIf env ann condition thenCase elseCase
    (MyPair ann a b) -> do
      inferA <- infer env a
      inferB <- infer env b
      let tyA = getTypeFromAnn inferA
          tyB = getTypeFromAnn inferB
      pure (MyPair (MTPair ann tyA tyB) inferA inferB)
    (MyArray ann items) -> do
      inferArray env ann items
    (MyConstructor ann modName name) -> do
      tyData <- inferDataConstructor env ann modName name
      pure (MyConstructor tyData modName name)
    (MyPatternMatch ann expr patterns) ->
      inferPatternMatch env ann expr patterns
