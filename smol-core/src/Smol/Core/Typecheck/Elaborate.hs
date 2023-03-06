{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Smol.Core.Typecheck.Elaborate
  ( elaborate,
    builtInTypes,
    getExprAnnotation,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.CPS
import Data.Foldable (foldl')
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Smol.Core.ExprUtils
import Smol.Core.Helpers
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Subtype
import Smol.Core.Typecheck.Types
import Smol.Core.Types

builtInTypes ::
  (Monoid ann, Ord (dep TypeName)) =>
  (forall a. a -> dep a) ->
  Map (dep TypeName) (DataType dep ann)
builtInTypes liftDep =
  let identityDt =
        DataType
          "Identity"
          ["a"]
          (M.singleton "Identity" [TVar mempty $ liftDep "a"])
      maybeDt =
        DataType
          "Maybe"
          ["a"]
          (M.fromList [("Just", [TVar mempty $ liftDep "a"]), ("Nothing", [])])
      eitherDt =
        DataType
          "Either"
          ["e", "a"]
          ( M.fromList
              [ ("Left", [TVar mempty $ liftDep "e"]),
                ("Right", [TVar mempty $ liftDep "a"])
              ]
          )

      theseDt =
        DataType
          "These"
          ["a", "b"]
          ( M.fromList
              [ ("This", [TVar mempty $ liftDep "a"]),
                ("That", [TVar mempty $ liftDep "b"]),
                ("These", [TVar mempty $ liftDep "a", TVar mempty $ liftDep "b"])
              ]
          )
      ordDt =
        DataType
          "Ord"
          []
          ( M.fromList [("LT", mempty), ("EQ", mempty), ("GT", mempty)]
          )
      listDt =
        DataType
          "List"
          ["a"]
          ( M.fromList
              [ ( "Cons",
                  [ TVar mempty (liftDep "a"),
                    TApp mempty (TConstructor mempty (liftDep "List")) (TVar mempty (liftDep "a"))
                  ]
                ),
                ("Nil", mempty)
              ]
          )
   in M.fromList
        [ (liftDep "Maybe", maybeDt),
          (liftDep "Either", eitherDt),
          (liftDep "Ord", ordDt),
          (liftDep "These", theseDt),
          (liftDep "Identity", identityDt),
          (liftDep "List", listDt)
        ]

elaborate ::
  ( Ord ann,
    Show ann,
    Monoid ann,
    MonadError (TCError ann) m
  ) =>
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
elaborate expr =
  fst
    <$> runReaderT
      ( runWriterT
          ( evalStateT
              (collectGlobals (infer expr))
              (TCState mempty 0 mempty)
          )
      )
      (TCEnv mempty mempty (builtInTypes emptyResolvedDep))

listenToGlobals ::
  ( MonadState (TCState ann) m,
    MonadError (TCError ann) m,
    MonadWriter [Substitution ResolvedDep ann] m,
    Eq ann,
    Show ann
  ) =>
  m a ->
  m (a, GlobalMap ann)
listenToGlobals f = do
  (a, globs) <- listenGlobals f
  combinedGlobs <- foldM combineTypeMaps mempty globs
  pure (a, combinedGlobs)

collectGlobals ::
  ( MonadState (TCState ann) m,
    MonadWriter [Substitution ResolvedDep ann] m,
    MonadError (TCError ann) m,
    Eq ann,
    Show ann
  ) =>
  m (ResolvedExpr (ResolvedType ann)) ->
  m (ResolvedExpr (ResolvedType ann))
collectGlobals f = do
  (expr, combinedGlobs) <- listenToGlobals f
  let ty = getExprAnnotation expr
      newTy =
        if globalMapIsNull combinedGlobs
          then ty
          else
            TGlobals
              (getTypeAnnotation ty)
              (getGlobalMap combinedGlobs)
              ty
  pure (mapOuterExprAnnotation (const newTy) expr)

inferInfix ::
  ( Ord ann,
    Show ann,
    MonadState (TCState ann) m,
    MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m,
    MonadWriter [Substitution ResolvedDep ann] m
  ) =>
  ann ->
  Op ->
  ResolvedExpr ann ->
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
inferInfix _ann OpAdd a b = do
  elabA <- infer a
  elabB <- infer b
  mostGeneralTy <-
    generaliseLiteral (getExprAnnotation elabA)
      `combine` generaliseLiteral (getExprAnnotation elabB)
  pure (EInfix mostGeneralTy OpAdd elabA elabB)
-- equality is certainly a big bucket of worms
-- for now, we'll concentrate on Int/Nat/Bool equality making sense
inferInfix ann OpEquals a b = do
  elabA <- infer a
  elabB <- infer b
  let tyA = generaliseLiteral (getExprAnnotation elabA)
      tyB = generaliseLiteral (getExprAnnotation elabB)

  -- throw if they're not the same
  _ <- tyA `isSubtypeOf` tyB

  -- check left is primitive
  when
    (typeIsStruct tyA)
    (throwError (TCCompoundTypeInEquality tyA))
  -- check right is primitive
  when
    (typeIsStruct tyB)
    (throwError (TCCompoundTypeInEquality tyB))
  pure (EInfix (TPrim ann TPBool) OpEquals elabA elabB)

typeLiteralFromPrim :: Prim -> TypeLiteral
typeLiteralFromPrim (PBool b) = TLBool b
typeLiteralFromPrim (PInt a) = TLInt a
typeLiteralFromPrim (PNat a) = TLInt (fromIntegral a)
typeLiteralFromPrim PUnit = TLUnit

-- | infer synthesizes values
-- from introduction forms
infer ::
  ( Ord ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m,
    MonadState (TCState ann) m,
    MonadWriter [Substitution ResolvedDep ann] m
  ) =>
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
infer inferExpr = do
  case inferExpr of
    (EPrim ann prim) ->
      pure (EPrim (TLiteral ann (typeLiteralFromPrim prim)) prim)
    (EAnn _ typ expr) -> do
      typedExpr <- check typ expr
      pure (EAnn (getExprAnnotation typedExpr) (typ $> typ) typedExpr)
    (EInfix ann op a b) ->
      inferInfix ann op a b
    (EVar _ ident) -> do
      typ <- lookupVar ident
      pure (EVar typ ident)
    (ELambda ann ident body) -> inferLambda ann ident body
    (ELet ann ident expr body) ->
      withRecursiveFn (ELambda ann ident body) expr $ do
        typedExpr <- infer expr
        typedBody <- withVar ident (getExprAnnotation typedExpr) (infer body)
        pure (ELet (getExprAnnotation typedBody) ident typedExpr typedBody)
    (EApp _ fn arg) -> inferApplication Nothing fn arg
    (EIf _ predExpr thenExpr elseExpr) -> do
      typedPred <- check (TPrim (getExprAnnotation predExpr) TPBool) predExpr
      typedThen <- infer thenExpr
      typedElse <- infer elseExpr
      agreedType <-
        getExprAnnotation typedThen
          `combine` getExprAnnotation typedElse
      pure (EIf agreedType typedPred typedThen typedElse)
    (ETuple ann fstExpr restExpr) -> do
      typedFst <- infer fstExpr
      typedRest <- traverse infer restExpr
      let typ =
            TTuple
              ann
              (getExprAnnotation typedFst)
              (getExprAnnotation <$> typedRest)
      pure $ ETuple typ typedFst typedRest
    (EGlobalLet _ann ident value rest) -> do
      ((tyVal, tyRest), globs) <- listenToGlobals $ do
        tyVal <- infer value
        tyRest <- withGlobal ident (getExprAnnotation tyVal) (infer rest)
        pure (tyVal, tyRest)
      tellGlobal (filterIdent ident globs)
      pure (EGlobalLet (getExprAnnotation tyRest) ident tyVal tyRest)
    (EGlobal _ ident) -> do
      globType <- lookupGlobal ident
      tellGlobal (GlobalMap (M.singleton ident globType)) -- 'raise' constraint
      pure $ EGlobal globType ident
    (ERecord ann items) -> do
      tyItems <- traverse infer items
      pure $ ERecord (TRecord ann (getExprAnnotation <$> tyItems)) tyItems
    (ERecordAccess _ expr ident) -> do
      typedExpr <- infer expr
      tyResult <- case getExprAnnotation typedExpr of
        TRecord _ tyItems -> case M.lookup ident tyItems of
          Just ty -> pure ty
          Nothing -> throwError (TCRecordMissingItems (S.singleton ident))
        other -> throwError (TCExpectedRecord other)
      pure (ERecordAccess tyResult typedExpr ident)
    (EPatternMatch _ matchExpr pats) -> do
      elabExpr <- infer matchExpr
      let withPair (pat, patExpr) = do
            (elabPat, newVars) <- checkPattern (getExprAnnotation elabExpr) pat
            elabPatExpr <- withNewVars newVars (infer patExpr)
            pure (elabPat, elabPatExpr)
      elabPats <- traverse withPair pats
      let allTypes = getExprAnnotation . snd <$> elabPats
      typ <- combineMany allTypes
      pure (EPatternMatch typ elabExpr elabPats)
    (EConstructor ann constructor) -> do
      (typeName, vars, _, args) <- lookupConstructor constructor
      tyConstructor <- typeForConstructor ann typeName vars args
      tyArgs <- popArgs (length args)
      let ty =
            foldl'
              (TApp ann)
              tyConstructor
              tyArgs
      pure (EConstructor (reduceType ty) constructor)

inferApplication ::
  ( Ord ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m,
    MonadState (TCState ann) m,
    MonadWriter [Substitution ResolvedDep ann] m
  ) =>
  Maybe (ResolvedType ann) ->
  ResolvedExpr ann ->
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
inferApplication maybeCheckType fn arg = withRecursiveFn fn arg $ do
  typedArg <- infer arg
  -- if we are applying to a variable, then we need to be a bit clever and
  -- do some substitution etc. if not, just infer it as usual and yolo
  let inferFn exprFn = do
        typedFn <- infer exprFn -- get type of fn
        freshTyVar <- freshen (getExprAnnotation typedFn)
        case freshTyVar of
          (TFunc tAnn tClosure tArg tBody) -> do
            -- if this is a func, it may be ready to be applied
            maybeArg <- popArg
            case maybeArg of
              Just pushedArg -> do
                -- this is a func looking to be applied
                (tyArg, subs) <- listen (pushedArg `isSubtypeOf` tArg)
                -- use substitutions to replace what we have learned
                let realType =
                      substituteMany
                        subs
                        (TFunc tAnn tClosure tyArg tBody)
                pure (mapOuterExprAnnotation (const realType) typedFn) -- replace type with clever one
              Nothing -> pure typedFn
          _ -> pure typedFn

  typedFn <-
    pushArg (getExprAnnotation typedArg)
      >> inferFn fn

  retType <- getApplyReturnType (getExprAnnotation typedFn)

  finalReturnType <- case maybeCheckType of
    Just typ -> retType `isSubtypeOf` typ
    Nothing -> pure retType

  pure (EApp finalReturnType typedFn typedArg)

-- | if a function is annotated, we can use it in it's own
-- body, for recursion
withRecursiveFn ::
  (MonadReader (TCEnv ann) m) =>
  ResolvedExpr ann ->
  ResolvedExpr ann ->
  m a ->
  m a
withRecursiveFn (ELambda _ ident _) (EAnn _ fnTyp _) =
  withVar ident fnTyp
withRecursiveFn _ _ = id

-- given the type of the expression in a pattern match,
-- check that the pattern makes sense with it
checkPattern ::
  ( Show ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m
  ) =>
  ResolvedType ann ->
  Pattern ResolvedDep ann ->
  m (Pattern ResolvedDep (ResolvedType ann), Map (ResolvedDep Identifier) (ResolvedType ann))
checkPattern checkTy checkPat = do
  case (checkTy, checkPat) of
    (TTuple _ tA tRest, PTuple ann pA pRest) | length tRest == length pRest -> do
      (patA, envA) <- checkPattern tA pA
      (patRest, envRest) <- neUnzip <$> neZipWithM checkPattern tRest pRest
      let ty = TTuple ann (getPatternAnnotation patA) (getPatternAnnotation <$> patRest)
          env = envA <> mconcat (NE.toList envRest)
      pure (PTuple ty patA patRest, env)
    (ty, PVar _ ident) ->
      pure (PVar ty ident, M.singleton ident ty)
    (ty, PWildcard _) -> pure (PWildcard ty, mempty)
    (ty@(TLiteral _ tPrim), PLiteral _ pPrim)
      | tPrim == typeLiteralFromPrim pPrim ->
          pure (PLiteral ty pPrim, mempty)
    (ty@(TPrim _ TPBool), PLiteral _ (PBool b)) ->
      pure (PLiteral ty (PBool b), mempty)
    (ty@(TPrim _ TPNat), PLiteral _ (PNat b)) ->
      pure (PLiteral ty (PNat b), mempty)
    (ty@(TPrim _ TPInt), PLiteral _ (PInt b)) ->
      pure (PLiteral ty (PInt b), mempty)
    (ty@(TUnion _ l r), pat@(PLiteral _ lit)) -> do
      result <-
        tryError
          ( checkPattern l pat -- is left OK?
              `catchError` \_ -> checkPattern r pat -- or maybe right is?
          )
      case result of
        Left _ -> throwError (TCPatternMismatch pat ty)
        Right _ -> pure (PLiteral ty lit, mempty)
    (ty, PConstructor ann constructor args) -> do
      -- we don't check the constructor is valid yet
      let flattened = flattenConstructorType ty

      -- lookup the constructor itself (ie, `Just`, `Nothing`)
      (patTypeName, _, otherConstructors, consArgs) <- lookupConstructor constructor

      case flattened of
        Left _ -> do
          -- don't know what it is... the following typechecks but is it nonsense
          (patArgs, envArgs) <-
            unzip <$> zipWithM checkPattern consArgs args

          -- check number of args matches what constructor expects
          when
            (length patArgs /= length consArgs)
            $ throwError
              (TCConstructorArgumentMismatch constructor (length consArgs) (length patArgs))

          let constructorTy = dataTypeWithVars ann patTypeName consArgs
          pure (PConstructor constructorTy constructor patArgs, mconcat envArgs)
        Right (typeName, tyArgs) -> do
          -- check constructor lives in type
          when (typeName /= patTypeName) $
            throwError (TCUnknownConstructor constructor otherConstructors)

          (patArgs, envArgs) <-
            unzip <$> zipWithM checkPattern tyArgs args

          -- check number of args matches what constructor expects
          when
            (length patArgs /= length consArgs)
            $ throwError
              (TCConstructorArgumentMismatch constructor (length consArgs) (length patArgs))

          let constructorTy = dataTypeWithVars ann patTypeName tyArgs
          pure (PConstructor constructorTy constructor patArgs, mconcat envArgs)
    (otherTy, otherPat) -> throwError (TCPatternMismatch otherPat otherTy)

checkLambda ::
  ( MonadState (TCState ann) m,
    MonadWriter [Substitution ResolvedDep ann] m,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m,
    Show ann,
    Ord ann
  ) =>
  ResolvedType ann ->
  ResolvedDep Identifier ->
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
checkLambda (TFunc tAnn _ tFrom tTo) ident body = do
  maybeArg <- popArg
  realFrom <- case maybeArg of
    Just arg -> arg `isSubtypeOf` tFrom
    Nothing -> pure tFrom
  (typedBody, typedClosure, subs) <- withVar ident realFrom $ do
    (tBody, subs) <- listen (check tTo body)
    tClosure <- M.delete (rdIdentifier ident) <$> getClosureType tBody
    pure (tBody, tClosure, subs)
  let lambdaType =
        substituteMany
          subs
          ( TFunc
              tAnn
              typedClosure
              realFrom
              (getExprAnnotation typedBody)
          )
  pure (ELambda lambdaType ident typedBody)
checkLambda other _ _ =
  throwError (TCExpectedFunction other)

inferLambda ::
  ( Ord ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m,
    MonadState (TCState ann) m,
    MonadWriter [Substitution ResolvedDep ann] m
  ) =>
  ann ->
  ResolvedDep Identifier ->
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
inferLambda ann ident body = do
  maybeTyp <- popArg
  tyArg <- case maybeTyp of
    Just typ -> pure typ
    Nothing -> getUnknown ann
  (typedBody, typedClosure, subs) <- withVar ident tyArg $ do
    (tBody, subs) <- listen (infer body)
    tClosure <- M.delete (rdIdentifier ident) <$> getClosureType tBody
    pure (tBody, tClosure, subs)
  let lambdaType =
        substituteMany
          subs
          (TFunc ann typedClosure tyArg (getExprAnnotation typedBody))
  pure (ELambda lambdaType ident typedBody)

-- | given an expected type
-- check it makes sense
check ::
  ( Ord ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m,
    MonadState (TCState ann) m,
    MonadWriter [Substitution ResolvedDep ann] m
  ) =>
  ResolvedType ann ->
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
check typ expr = do
  case expr of
    ELambda _ ident body ->
      checkLambda typ ident body
    EIf _ predExpr thenExpr elseExpr -> do
      typedPred <- check (TPrim (getExprAnnotation predExpr) TPBool) predExpr
      typedThen <- check typ thenExpr
      typedElse <- check typ elseExpr
      pure (EIf typ typedPred typedThen typedElse)
    EApp _ fn arg -> inferApplication (Just typ) fn arg
    ETuple _ fstExpr restExpr ->
      case typ of
        TTuple tAnn tFst tRest -> do
          when
            (length tRest /= length restExpr)
            (throwError $ TCTupleSizeMismatch (length restExpr + 1) typ)
          typedFst <- check tFst fstExpr
          typedRest <- neZipWithM check tRest restExpr
          realType <-
            TTuple tAnn (getExprAnnotation typedFst) (getExprAnnotation <$> typedRest) `isSubtypeOf` typ
          pure $ ETuple realType typedFst typedRest
        _ -> throwError (TCExpectedTuple typ)
    EInfix _ OpAdd a b -> do
      elabA <- check typ a
      elabB <- check typ b
      pure (EInfix typ OpAdd elabA elabB)
    EGlobal _ ident -> do
      tellGlobal (GlobalMap (M.singleton ident typ)) -- 'raise' constraint
      pure (EGlobal typ ident)
    EPatternMatch _ matchExpr pats -> do
      elabExpr <- infer matchExpr
      let withPair (pat, patExpr) = do
            (elabPat, newVars) <- checkPattern (getExprAnnotation elabExpr) pat
            elabPatExpr <- withNewVars newVars (check typ patExpr)
            pure (elabPat, elabPatExpr)
      elabPats <- traverse withPair pats
      pure (EPatternMatch typ elabExpr elabPats)
    other -> do
      inferredExpr <- infer other
      (realType, subs) <- listen (getExprAnnotation inferredExpr `isSubtypeOf` typ)
      pure $ inferredExpr $> substituteMany subs realType
