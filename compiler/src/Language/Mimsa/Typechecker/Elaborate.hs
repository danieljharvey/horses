{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Elaborate
  ( elab,
    recoverAnn,
    getTypeFromAnn,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State)
import Control.Monad.Writer
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.Exhaustiveness
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type ElabM = ExceptT TypeError (WriterT [Constraint] (ReaderT Swaps (State TypecheckState)))

type TcExpr = Expr Variable Annotation

recoverAnn :: MonoType -> Annotation
recoverAnn = getAnnotationForType

getTypeFromAnn :: Expr var MonoType -> MonoType
getTypeFromAnn = getAnnotation

getPatternTypeFromAnn :: Pattern Variable MonoType -> MonoType
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

getSpreadTypeFromAnn :: Spread Variable MonoType -> Maybe MonoType
getSpreadTypeFromAnn (SpreadValue ann _) = Just ann
getSpreadTypeFromAnn _ = Nothing

type ElabExpr = Expr Variable MonoType

instantiate ::
  Scheme ->
  ElabM MonoType
instantiate (Scheme vars ty) = do
  newVars <- traverse (const $ getUnknown mempty) vars
  let pairs = zip vars newVars
  let subst = Substitutions $ M.fromList pairs
  pure (applySubst subst ty)

--------------

elabLiteral :: Annotation -> Literal -> ElabM ElabExpr
elabLiteral ann lit =
  let tyLit = case lit of
        (MyInt _) -> MTInt
        (MyBool _) -> MTBool
        (MyString _) -> MTString
   in pure (MyLiteral (MTPrim ann tyLit) lit)

elabVarFromScope ::
  Environment ->
  Annotation ->
  Variable ->
  ElabM ElabExpr
elabVarFromScope env@(Environment env' _ _) ann var' =
  case M.lookup (variableToTypeIdentifier var') env' of
    Just mt -> do
      freshMonoType <- instantiate mt
      pure (MyVar freshMonoType var')
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

lookupInfixOp ::
  Environment ->
  Annotation ->
  InfixOp ->
  ElabM MonoType
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

-- when we come to do let recursive the name of our binder
-- may already be turned into a number in the expr
-- so we look it up to make sure we bind the right thing
findActualBindingInSwaps :: Variable -> ElabM Variable
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
elabLetBinding ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabLetBinding env ann binder expr body = do
  if bindingIsRecursive binder expr
    then elabRecursiveLetBinding env ann binder expr body
    else do
      -- we have to run substitutions on this before "saving" it
      (elabExpr, constraints) <- listen (elab env expr)
      subst <- solve constraints
      let tySubstExpr = applySubst subst (getTypeFromAnn elabExpr)
      let newEnv =
            envFromVar binder (generalise env tySubstExpr)
              <> env
      elabBody <- elab newEnv body
      pure
        ( MyLet
            (getTypeFromAnn elabBody)
            binder
            elabExpr
            elabBody
        )

elabRecursiveLetBinding ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabRecursiveLetBinding env ann binder expr body = do
  binderInExpr <- findActualBindingInSwaps binder
  exprAsFix <- elab env (MyLambda mempty binderInExpr expr)
  tyRec <- getUnknown ann
  tyExpr <- getUnknown ann
  let tyExprAsFix = getTypeFromAnn exprAsFix
  tell
    [ ShouldEqual tyExprAsFix (MTFunction mempty tyRec tyRec),
      ShouldEqual tyExprAsFix (MTFunction ann tyRec tyExpr)
    ]
  let newEnv2 =
        envFromVar binder (Scheme [] tyRec)
          <> env
  -- TODO: is this bad
  elabExpr <- elab newEnv2 expr
  elabBody <- elab newEnv2 body
  pure (MyLet (getTypeFromAnn elabBody) binder elabExpr elabBody)

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
  [(Pattern Variable Annotation, TcExpr)] ->
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
  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann (fst <$> patterns)
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
  Pattern Variable Annotation ->
  ElabM (Pattern Variable MonoType, Environment)
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
  (mt, elabA, elabB) <- elabInfix env (MTArray ann (MTVar mempty (TVName "a"))) a b
  pure (MyInfix mt ArrayConcat elabA elabB)
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
  Pattern Variable Annotation ->
  TcExpr ->
  TcExpr ->
  ElabM ElabExpr
elabLetPattern env ann pat expr body = do
  (elabPat, newEnv) <- elabPattern env pat
  elabExpr <- elab env expr
  elabBody <- elab newEnv body
  tell
    [ ShouldEqual (getPatternTypeFromAnn elabPat) (getTypeFromAnn elabExpr)
    ]

  -- perform exhaustiveness checking at end so it doesn't mask more basic errors
  validatePatterns env ann [pat]
  pure (MyLetPattern (getTypeFromAnn elabBody) elabPat elabExpr elabBody)

elabLambda ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  ElabM ElabExpr
elabLambda env ann binder body = do
  tyBinder <- getUnknown ann
  let tmpCtx =
        envFromVar binder (Scheme [] tyBinder) <> env
  elabBody <- elab tmpCtx body
  let tyReturn = MTFunction ann tyBinder (getTypeFromAnn elabBody)
  pure (MyLambda tyReturn binder elabBody)

isTwoArityFunction :: MonoType -> Bool
isTwoArityFunction (MTFunction _ _ MTFunction {}) = True
isTwoArityFunction _ = False

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
  let arityError =
        FunctionArityMismatch
          (getAnnotationForType . getAnnotation $ elabBindExpr)
          2
          tyBind
  tell
    [ ShouldEqual
        tyBind
        ( MTFunction
            mempty
            u1
            (MTFunction mempty u2 u3)
        )
    ]
  let newEnv = envFromInfixOp infixOp tyBind <> env
  if isTwoArityFunction tyBind
    then elab newEnv expr
    else throwError arityError

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
    (MyTypedHole ann name) -> do
      tyHole <- addTypedHole ann name
      pure (MyVar tyHole (NamedVar name))
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
