{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Typechecker.Elaborate
  ( elab,
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
import qualified Data.Map as M
import Language.Mimsa.ExprUtils
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.Exhaustiveness
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.ScopeTypeVar
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
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
    PConstructor ann _ _ -> ann
    PPair ann _ _ -> ann
    PRecord ann _ -> ann
    PArray ann _ _ -> ann
    PString ann _ _ -> ann

getSpreadTypeFromAnn :: Spread (Name, Unique) MonoType -> Maybe MonoType
getSpreadTypeFromAnn (SpreadValue ann _) = Just ann
getSpreadTypeFromAnn _ = Nothing

type ElabExpr = Expr (Name, Unique) MonoType

--------------

elabLiteral :: Annotation -> Literal -> ElabM ElabExpr
elabLiteral ann lit =
  let tyLit = case lit of
        (MyInt _) -> MTInt
        (MyBool _) -> MTBool
        (MyString _) -> MTString
   in pure (MyLiteral (MTPrim ann tyLit) lit)

lookupInEnv :: (Name, Unique) -> Environment -> Maybe Scheme
lookupInEnv var' (Environment env' _ _ _) =
  let look v = M.lookup v env'
   in look (variableToTypeIdentifier var')

elabVarFromScope ::
  Environment ->
  Annotation ->
  (Name, Unique) ->
  ElabM ElabExpr
elabVarFromScope env ann var' = do
  case lookupInEnv var' env of
    Just mt -> do
      freshMonoType <- instantiate ann mt
      pure (MyVar freshMonoType var')
    _ -> do
      throwError $
        NameNotFoundInScope
          ann
          undefined --(S.fromList (M.keys (getSchemes env)))
          undefined -- (fst var')

envFromVar :: (Name, Unique) -> Scheme -> Environment
envFromVar binder scheme =
  Environment (M.singleton (variableToTypeIdentifier binder) scheme) mempty mempty mempty

envFromInfixOp :: InfixOp -> Scheme -> Environment
envFromInfixOp infixOp scheme =
  Environment
    mempty
    mempty
    (M.singleton infixOp scheme)
    mempty

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
elabApplication ::
  Environment ->
  Annotation ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabApplication env ann function argument = do
  tyRes <- getUnknown ann
  function' <- elab env function
  argument' <- elab env argument
  tell
    [ ShouldEqual
        (getTypeFromAnn function')
        (MTFunction ann (getTypeFromAnn argument') tyRes)
    ]
  pure (MyApp tyRes function' argument')

bindingIsRecursive :: Identifier (Name, Unique) ann -> TcExpr -> Bool
bindingIsRecursive ident = getAny . withMonoid findBinding
  where
    variable = case ident of
      (Identifier _ a) -> a
      (AnnotatedIdentifier _ a) -> a
    findBinding (MyVar _ binding) | binding == variable = (False, Any True)
    findBinding _ = (True, mempty)

binderFromIdentifier :: Identifier var ann -> var
binderFromIdentifier = \case
  (Identifier _ a) -> a
  (AnnotatedIdentifier _ a) -> a

annotationFromIdentifier :: Identifier var ann -> ann
annotationFromIdentifier = \case
  (Identifier ann _) -> ann
  (AnnotatedIdentifier mt _) -> getAnnotationForType mt

monoTypeFromIdentifier :: Identifier var Annotation -> Maybe MonoType
monoTypeFromIdentifier = \case
  (AnnotatedIdentifier mt _) -> Just mt
  _ -> Nothing

-- if type is recursive we make it monomorphic
-- if not, we make it polymorphic
elabLetBinding ::
  Environment ->
  Annotation ->
  Identifier (Name, Unique) Annotation ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabLetBinding env ann ident expr body = do
  if bindingIsRecursive ident expr
    then elabRecursiveLetBinding env ann ident expr body
    else do
      let bindAnn = annotationFromIdentifier ident
          bindName = binderFromIdentifier ident
      -- we have to run substitutions on this before "saving" it
      (elabExpr, constraints) <- listen (elab env expr)
      subst <- solve constraints
      -- compare annotated type with elabbed expr if possible
      case monoTypeFromIdentifier ident of
        (Just mt) ->
          tell
            [ShouldEqual mt (getTypeFromAnn elabExpr)]
        _ -> pure ()
      let tySubstExpr = applySubst subst (getTypeFromAnn elabExpr)
      let newEnv =
            envFromVar bindName (generalise env tySubstExpr)
              <> env
      elabBody <- elab newEnv body
      pure
        ( MyLet
            (getTypeFromAnn elabBody $> ann) -- we want to make sure we keep the original source location
            (ident $> (tySubstExpr $> bindAnn))
            elabExpr
            elabBody
        )

elabRecursiveLetBinding ::
  Environment ->
  Annotation ->
  Identifier (Name, Unique) Annotation ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabRecursiveLetBinding env ann ident expr body = do
  let bindName = binderFromIdentifier ident
      bindAnn = annotationFromIdentifier ident
  tyRecExpr <- getUnknown ann
  let envWithRecursiveFn = envFromVar bindName (Scheme [] tyRecExpr) <> env
  elabExpr <- elab envWithRecursiveFn expr

  -- compare annotated type with elabbed expr if possible
  case monoTypeFromIdentifier ident of
    (Just mt) ->
      tell
        [ShouldEqual mt (getTypeFromAnn elabExpr)]
    _ -> pure ()

  let tyExpr = getTypeFromAnn elabExpr
      newEnv =
        envFromVar bindName (Scheme [] tyExpr) <> env

  elabBody <- elab newEnv body
  tell [ShouldEqual tyRecExpr (getTypeFromAnn elabExpr)]

  pure
    ( MyLet
        (getTypeFromAnn elabBody)
        (ident $> (tyExpr $> bindAnn))
        elabExpr
        elabBody
    )

elabIf :: Environment -> TcExpr -> TcExpr -> TcExpr -> ElabM ElabExpr
elabIf env condition thenExpr elseExpr = do
  condExpr <- elab env condition
  thenExpr' <- elab env thenExpr
  elseExpr' <- elab env elseExpr
  tell
    [ ShouldEqual
        (getTypeFromAnn thenExpr')
        (getTypeFromAnn elseExpr'),
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
elabPatternMatch ::
  Environment ->
  Annotation ->
  TcExpr ->
  [(Pattern (Name, Unique) Annotation, TcExpr)] ->
  ElabM ElabExpr
elabPatternMatch env ann expr patterns = do
  -- ensure we even have any patterns to match on
  nePatterns <- checkEmptyPatterns ann patterns
  -- elaborate source expression that we are matching
  elabExpr <- elab env expr
  -- elab types of all patterns
  elabPatterns <-
    traverse
      ( \(pat, patternExpr) -> do
          (elabPat, newEnv) <- elabPattern env pat
          tell
            [ ShouldEqual
                (getPatternTypeFromAnn elabPat)
                (getTypeFromAnn elabExpr)
            ]
          tyPatternExpr <- elab newEnv patternExpr
          pure (elabPat, tyPatternExpr)
      )
      nePatterns
  -- combine all patterns to check their types match
  tyMatchedPattern <- matchList (getPatternTypeFromAnn . fst <$> elabPatterns)
  -- match patterns with match expr
  tell [ShouldEqual tyMatchedPattern (getTypeFromAnn elabExpr)]
  -- combine all output expr types
  tyMatchedExprs <- matchList (getTypeFromAnn . snd <$> elabPatterns)
  -- remove (,unique) from var
  let patternsWithoutUnique = first fst . fst <$> patterns
  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann patternsWithoutUnique
  -- wrap up the pattern match again
  pure
    ( MyPatternMatch
        tyMatchedExprs
        elabExpr
        (NE.toList elabPatterns)
    )

-- get non-empty list from list and error if not
checkEmptyPatterns :: Annotation -> [a] -> ElabM (NE.NonEmpty a)
checkEmptyPatterns ann as = case as of
  [] -> throwError (PatternMatchErr $ EmptyPatternMatch ann)
  other -> pure (NE.fromList other)

elabPattern ::
  Environment ->
  Pattern (Name, Unique) Annotation ->
  ElabM (Pattern (Name, Unique) MonoType, Environment)
elabPattern env (PLit ann lit) = do
  elabExpr <- elab env (MyLiteral ann lit)
  pure
    ( PLit (getTypeFromAnn elabExpr) lit,
      env
    )
elabPattern env (PVar ann binder) = do
  tyBinder <- getUnknown ann
  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env
  pure
    ( PVar tyBinder binder,
      tmpCtx
    )
elabPattern env (PWildcard ann) = do
  tyUnknown <- getUnknown ann
  pure
    ( PWildcard tyUnknown,
      env
    )
elabPattern env (PConstructor ann tyCon args) = do
  elabEverything <- traverse (elabPattern env) args
  let elabArgs = fst <$> elabEverything
  let newEnv = mconcat (snd <$> elabEverything) <> env
  dt@(DataType ty _ _) <- lookupConstructor newEnv ann tyCon
  -- we get the types for the constructor in question
  -- and unify them with the tests in the pattern
  consType <- inferConstructorTypes dt
  tyTypeVars <- case M.lookup tyCon (snd consType) of
    Just (TypeConstructor _ dtTypeVars tyDtArgs) -> do
      let tyPairs = zip (getPatternTypeFromAnn <$> elabArgs) tyDtArgs
      traverse_ (\(a, b) -> tell [ShouldEqual a b]) tyPairs
      pure dtTypeVars
    _ -> throwError UnknownTypeError
  checkArgsLength ann dt tyCon elabArgs
  pure
    ( PConstructor (dataTypeWithVars ann ty tyTypeVars) tyCon elabArgs,
      newEnv
    )
elabPattern env (PPair ann a b) = do
  (elabA, envA) <- elabPattern env a
  (elabB, envB) <- elabPattern envA b
  pure
    ( PPair
        ( MTPair
            ann
            (getPatternTypeFromAnn elabA)
            (getPatternTypeFromAnn elabB)
        )
        elabA
        elabB,
      envB
    )
elabPattern env (PRecord ann items) = do
  let elabRow (k, v) = do
        (tyValue, envNew) <- elabPattern env v
        pure (M.singleton k tyValue, envNew)
  elabEverything <- traverse elabRow (M.toList items)
  let elabItems = mconcat (fst <$> elabEverything)
  let newEnv = mconcat (snd <$> elabEverything) <> env
  tyRest <- getUnknown ann
  pure
    ( PRecord
        ( MTRecordRow
            ann
            (getPatternTypeFromAnn <$> elabItems)
            tyRest
        )
        elabItems,
      newEnv
    )
elabPattern env (PArray ann items spread) = do
  elabEverything <- traverse (elabPattern env) items
  (elabSpread, env2) <- case spread of
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
    ( (getPatternTypeFromAnn . fst <$> elabEverything)
        <> maybe mempty pure (getSpreadTypeFromAnn elabSpread)
    ) of
    Just neItems -> matchList neItems
    _ -> getUnknown ann
  let newEnv = mconcat (snd <$> elabEverything) <> env2
  pure
    ( PArray
        ( MTArray
            ann
            tyItems
        )
        (fst <$> elabEverything)
        elabSpread,
      newEnv
    )
elabPattern env (PString ann a as) = do
  let envFromStrPart x = case x of
        (StrValue ann' name) ->
          envFromVar name (Scheme [] (MTPrim ann' MTString))
        _ -> mempty
  let newEnv = envFromStrPart a <> envFromStrPart as <> env
  let elabStringPart (StrValue ann' name) =
        StrValue (MTPrim ann' MTString) name
      elabStringPart (StrWildcard ann') =
        StrWildcard (MTPrim ann' MTString)
  pure
    ( PString
        (MTPrim ann MTString)
        (elabStringPart a)
        (elabStringPart as),
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

elabOperator ::
  Environment ->
  Annotation ->
  Operator ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabOperator env ann Equals a b = do
  elabA <- elab env a
  elabB <- elab env b
  let tyA = getTypeFromAnn elabA
      tyB = getTypeFromAnn elabB
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
            elabA
            elabB
        )
elabOperator env ann Add a b = do
  (mt, elabA, elabB) <- elabInfix env (MTPrim ann MTInt) a b
  pure (MyInfix mt Add elabA elabB)
elabOperator env ann Subtract a b = do
  (mt, elabA, elabB) <- elabInfix env (MTPrim ann MTInt) a b
  pure (MyInfix mt Subtract elabA elabB)
elabOperator env ann StringConcat a b = do
  (mt, elabA, elabB) <- elabInfix env (MTPrim ann MTString) a b
  pure (MyInfix mt StringConcat elabA elabB)
elabOperator env ann ArrayConcat a b = do
  tyArr <- getUnknown ann
  (mt, elabA, elabB) <- elabInfix env (MTArray ann tyArr) a b
  pure (MyInfix mt ArrayConcat elabA elabB)
elabOperator env ann GreaterThan a b = do
  (mt, elabA, elabB) <-
    elabComparison
      env
      (MTPrim ann MTInt)
      (MTPrim ann MTBool)
      a
      b
  pure (MyInfix mt GreaterThan elabA elabB)
elabOperator env ann GreaterThanOrEqualTo a b = do
  (mt, elabA, elabB) <-
    elabComparison
      env
      (MTPrim ann MTInt)
      (MTPrim ann MTBool)
      a
      b
  pure (MyInfix mt GreaterThanOrEqualTo elabA elabB)
elabOperator env ann LessThan a b = do
  (mt, elabA, elabB) <-
    elabComparison
      env
      (MTPrim ann MTInt)
      (MTPrim ann MTBool)
      a
      b
  pure (MyInfix mt LessThan elabA elabB)
elabOperator env ann LessThanOrEqualTo a b = do
  (mt, elabA, elabB) <-
    elabComparison
      env
      (MTPrim ann MTInt)
      (MTPrim ann MTBool)
      a
      b
  pure (MyInfix mt LessThanOrEqualTo elabA elabB)
elabOperator env ann (Custom infixOp) a b = do
  tyRes <- getUnknown ann
  tyFun <- lookupInfixOp env ann infixOp
  elabA <- elab env a
  elabB <- elab env b
  tell
    [ ShouldEqual
        tyFun
        ( MTFunction
            ann
            (getTypeFromAnn elabA)
            (MTFunction ann (getTypeFromAnn elabB) tyRes)
        )
    ]
  pure (MyInfix tyRes (Custom infixOp) elabA elabB)

-- | infix operator where inputs and output are the same
elabInfix ::
  Environment ->
  MonoType ->
  TcExpr ->
  TcExpr ->
  ElabM (MonoType, ElabExpr, ElabExpr)
elabInfix env mt a b = do
  elabA <- elab env a
  elabB <- elab env b
  let tyA = getTypeFromAnn elabA
      tyB = getTypeFromAnn elabB
  tell [ShouldEqual tyA tyB, ShouldEqual tyB mt, ShouldEqual tyA mt]
  pure (mt, elabA, elabB)

-- | infix operator where inputs match but output could be different
-- | for instance, 1 < 2 == True would be `Int -> Int -> Bool`
elabComparison ::
  Environment ->
  MonoType ->
  MonoType ->
  TcExpr ->
  TcExpr ->
  ElabM (MonoType, ElabExpr, ElabExpr)
elabComparison env inputMt outputMt a b = do
  elabA <- elab env a
  elabB <- elab env b
  let tyA = getTypeFromAnn elabA
      tyB = getTypeFromAnn elabB
  tell
    [ ShouldEqual tyA tyB,
      ShouldEqual tyA inputMt,
      ShouldEqual tyB inputMt
    ]
  pure (outputMt, elabA, elabB)

elabRecordAccess ::
  Environment ->
  Annotation ->
  TcExpr ->
  Name ->
  ElabM ElabExpr
elabRecordAccess env ann a name = do
  elabItems <- elab env a
  let elabRow = \case
        (MTRecord _ bits) ->
          case M.lookup name bits of
            Just mt -> pure mt
            _ ->
              throwError $ MissingRecordTypeMember ann name bits
        (MTRecordRow _ as rest) ->
          case M.lookup name as of
            Just mt -> pure mt
            _ -> elabRow rest
        (MTVar ann' _) -> do
          tyRest <- getUnknown ann'
          tyItem <- getUnknown ann'
          tell
            [ ShouldEqual
                (getTypeFromAnn elabItems)
                ( MTRecordRow
                    ann'
                    (M.singleton name tyItem)
                    tyRest
                )
            ]
          pure tyItem
        _ -> throwError $ CannotMatchRecord env ann (getTypeFromAnn elabItems)
  mt <- elabRow (getTypeFromAnn elabItems)
  pure (MyRecordAccess mt elabItems name)

elabLetPattern ::
  Environment ->
  Annotation ->
  Pattern (Name, Unique) Annotation ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabLetPattern env ann pat expr body = do
  elabExpr <- elab env expr
  (elabPat, newEnv) <- elabPattern env pat
  elabBody <- elab newEnv body
  tell
    [ ShouldEqual (getPatternTypeFromAnn elabPat) (getTypeFromAnn elabExpr)
    ]

  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann [first fst pat]

  pure (MyLetPattern (getTypeFromAnn elabBody) elabPat elabExpr elabBody)

elabLambda ::
  Environment ->
  Annotation ->
  Identifier (Name, Unique) Annotation ->
  TcExpr ->
  ElabM ElabExpr
elabLambda env ann ident body = do
  let binder = binderFromIdentifier ident
      bindAnn = annotationFromIdentifier ident

  -- compare annotated type with elabbed expr if possible
  tyBinder <- case monoTypeFromIdentifier ident of
    (Just mt) -> pure mt
    Nothing -> getUnknown bindAnn

  (newEnv, tyBinder') <- freshNamedType env tyBinder

  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder') <> newEnv

  elabBody <- elab tmpCtx body
  let tyReturn = MTFunction ann tyBinder' (getTypeFromAnn elabBody)
  pure (MyLambda tyReturn (ident $> (tyBinder' $> bindAnn)) elabBody)

elabDefineInfix ::
  Environment ->
  Annotation ->
  InfixOp ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabDefineInfix env ann infixOp infixExpr expr = do
  u1 <- getUnknown ann
  u2 <- getUnknown ann
  u3 <- getUnknown ann
  elabBindExpr <- elab env infixExpr
  let tyBind = getTypeFromAnn elabBindExpr
  tell
    [ ShouldEqual
        tyBind
        ( MTFunction
            mempty
            u1
            (MTFunction mempty u2 u3)
        )
    ]
  let newEnv = envFromInfixOp infixOp (generalise env tyBind) <> env
  elabBodyExpr <- elab newEnv expr
  pure $ MyDefineInfix (getTypeFromAnn elabBodyExpr) infixOp elabBindExpr elabBodyExpr

elabArray ::
  Environment ->
  Annotation ->
  [TcExpr] ->
  ElabM ElabExpr
elabArray env ann items = do
  elabItems <- traverse (elab env) items
  tyItems <- case NE.nonEmpty elabItems of
    Just neElabItems -> matchList (getTypeFromAnn <$> neElabItems)
    Nothing -> getUnknown ann
  pure (MyArray (MTArray ann tyItems) elabItems)

elab ::
  Environment ->
  TcExpr ->
  ElabM ElabExpr
elab env elabExpr =
  case elabExpr of
    (MyLiteral ann a) -> elabLiteral ann a
    (MyVar ann name) ->
      elabVarFromScope env ann name
    (MyRecord ann map') -> do
      elabItems <- traverse (elab env) map'
      let tyItems = getTypeFromAnn <$> elabItems
      pure (MyRecord (MTRecord ann tyItems) elabItems)
    (MyInfix ann op a b) -> elabOperator env ann op a b
    (MyTypedHole ann (name, unique)) -> do
      tyHole <- addTypedHole env ann name
      pure (MyVar tyHole (name, unique))
    (MyLet ann binder expr body) ->
      elabLetBinding env ann binder expr body
    (MyLetPattern ann pat expr body) ->
      elabLetPattern env ann pat expr body
    (MyRecordAccess ann (MyRecord ann' items') name) ->
      elabRecordAccess env ann (MyRecord ann' items') name
    (MyRecordAccess ann a name) ->
      elabRecordAccess env ann a name
    (MyLambda ann binder body) ->
      elabLambda env ann binder body
    (MyApp ann function argument) ->
      elabApplication env ann function argument
    (MyIf _ condition thenCase elseCase) ->
      elabIf env condition thenCase elseCase
    (MyPair ann a b) -> do
      elabA <- elab env a
      elabB <- elab env b
      let tyA = getTypeFromAnn elabA
          tyB = getTypeFromAnn elabB
      pure (MyPair (MTPair ann tyA tyB) elabA elabB)
    (MyData ann dataType expr) -> do
      newEnv <- storeDataDeclaration env ann dataType
      innerExpr <- elab newEnv expr
      pure (MyData (getTypeFromAnn innerExpr) dataType innerExpr)
    (MyArray ann items) -> do
      elabArray env ann items
    (MyConstructor ann name) -> do
      tyData <- inferDataConstructor env ann name
      pure (MyConstructor tyData name)
    (MyDefineInfix ann infixOp infixExpr expr) ->
      elabDefineInfix env ann infixOp infixExpr expr
    (MyPatternMatch ann expr patterns) ->
      elabPatternMatch env ann expr patterns
