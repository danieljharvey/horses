{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Smol.Core.Typecheck.Elaborate
  ( elaborate,
    getExprAnnotation,
    checkPattern,
  )
where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Control.Monad.Writer.CPS
import Data.Foldable (toList, traverse_)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Smol.Core.ExprUtils
import Smol.Core.Helpers
import Smol.Core.Typecheck.Pattern (checkPattern)
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Simplify
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Subtype
import Smol.Core.Typecheck.Typeclass.Helpers
import Smol.Core.Typecheck.Types
import Smol.Core.Types

elaborate ::
  ( Ord ann,
    Show ann,
    Monoid ann,
    MonadError (TCError ann) m
  ) =>
  TCEnv ann ->
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann), M.Map (ResolvedDep Identifier) (Constraint ResolvedDep ann))
elaborate env expr =
  runReaderT
    ( runWriterT
        ( evalStateT
            (infer expr)
            (TCState mempty 0 mempty)
        )
    )
    env
    >>= \(typedExpr, events) -> do
      -- apply substitutions
      let simpleExpr = simplifyType . substituteMany (filterSubstitutions events) <$> typedExpr

      let typeclassUses = recoverTypeclassUses events

      -- lookup typeclasses we need, explode if they're missing
      traverse_ (lookupTypeclassConstraint env) (M.elems typeclassUses)

      pure (simpleExpr, typeclassUses)

inferInfix ::
  ( Ord ann,
    Show ann,
    MonadState (TCState ann) m,
    MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m,
    MonadWriter [TCWrite ann] m
  ) =>
  ann ->
  Op ->
  ResolvedExpr ann ->
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
inferInfix ann OpAdd a b = do
  elabA <- infer a
  elabB <- infer b
  let tyA = getExprAnnotation elabA
      tyB = getExprAnnotation elabB

  -- throw if these things are totally incompatible (we use `censor` to stop
  -- "learning" anything about the oversimplified types)
  _ <-
    censor (const mempty) $
      generaliseLiteral tyA `isSubtypeOf` generaliseLiteral tyB

  let addTy = TInfix ann OpAdd tyA tyB

  pure (EInfix addTy OpAdd elabA elabB)
-- equality is certainly a big bucket of worms
-- for now, we'll concentrate on Int/Nat/Bool equality making sense
inferInfix ann OpEquals a b = do
  elabA <- infer a
  elabB <- infer b
  let tyA = getExprAnnotation elabA
      tyB = getExprAnnotation elabB

  -- throw if they're not the same
  _ <- generaliseLiteral tyA `isSubtypeOf` generaliseLiteral tyB

  -- check left is primitive
  when
    (typeIsStruct (simplifyType tyA))
    (throwError (TCCompoundTypeInEquality tyA))
  -- check right is primitive
  when
    (typeIsStruct (simplifyType tyB))
    (throwError (TCCompoundTypeInEquality tyB))
  pure (EInfix (TInfix ann OpEquals tyA tyB) OpEquals elabA elabB)

-- | infer synthesizes values
-- from introduction forms
infer ::
  ( Ord ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m,
    MonadState (TCState ann) m,
    MonadWriter [TCWrite ann] m
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
    (EVar ann ident) -> do
      typ <- lookupVar ann ident
      pure (EVar typ ident)
    (ELambda ann ident body) -> inferLambda ann ident body
    (ELet ann ident expr body) ->
      withRecursiveFn (ELambda ann ident body) expr $ do
        tyUnknown <- getUnknown ann
        typedExpr <- withVar ident tyUnknown (infer expr)
        let tyExpr =
              substituteMany
                [ Substitution
                    (SubType tyUnknown)
                    (getExprAnnotation typedExpr)
                ]
                (getExprAnnotation typedExpr)
        typedBody <- withVar ident tyExpr (infer body)
        pure $ ELet (getExprAnnotation typedBody) ident typedExpr typedBody
    (EApp ann fn arg) -> inferApplication ann Nothing fn arg
    (EIf _ predExpr thenExpr elseExpr) -> do
      typedPred <- check (TPrim (getExprAnnotation predExpr) TPBool) predExpr
      typedThen <- infer thenExpr
      typedElse <- infer elseExpr
      agreedType <-
        combineMany
          ( getExprAnnotation typedThen
              NE.:| [ getExprAnnotation typedElse
                    ]
          )
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
    (EArray ann as) -> do
      typedAs <- traverse infer as
      let size = fromIntegral (length as)
      ty <- case NE.nonEmpty (reverse $ toList typedAs) of
        Nothing -> error "what type is empty list"
        Just tyAs -> combineMany (getExprAnnotation <$> tyAs)
      pure (EArray (TArray ann size ty) typedAs)
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

      pure (EConstructor tyConstructor constructor)

inferApplication ::
  ( Ord ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m,
    MonadState (TCState ann) m,
    MonadWriter [TCWrite ann] m
  ) =>
  ann ->
  Maybe (ResolvedType ann) ->
  ResolvedExpr ann ->
  ResolvedExpr ann ->
  m (ResolvedExpr (ResolvedType ann))
inferApplication ann maybeCheckType fn arg = withRecursiveFn fn arg $ do
  typedArg <- infer arg

  -- if we are applying to a variable, then we need to be a bit clever and
  -- do some substitution etc. if not, just infer it as usual and yolo
  let inferFn exprFn = do
        typedFn <- infer exprFn -- get type of fn
        (freshTyVar, undoSubs) <- freshen (getExprAnnotation typedFn)

        case freshTyVar of
          (TFunc tAnn tClosure tArg tBody) -> do
            -- if this is a func, it may be ready to be applied
            maybeArg <- popArg
            case maybeArg of
              Just pushedArg -> do
                -- this is a func looking to be applied
                (tyArg, subs) <- listen (pushedArg `isSubtypeOf` tArg)

                -- use substitutions to replace what we have learned
                -- the `undoSubs` put back any generalised vars to what they
                -- were so that any unchanged type vars don't get turned into
                -- unknowns unnecessarily
                let realType =
                      substituteMany
                        (filterSubstitutions subs <> undoSubs)
                        (TFunc tAnn tClosure tyArg tBody)

                pure (mapOuterExprAnnotation (const realType) typedFn) -- replace type with clever one
              Nothing -> pure typedFn
          _ -> pure typedFn

  typedFn <-
    pushArg (getExprAnnotation typedArg)
      >> inferFn fn

  maybeReturnType <- getApplyReturnType (getExprAnnotation typedFn)

  finalReturnType <- case maybeReturnType of
    Nothing -> case maybeCheckType of
      Just typ -> pure typ
      Nothing -> getUnknown ann
    Just retType -> case maybeCheckType of
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

checkLambda ::
  ( MonadState (TCState ann) m,
    MonadWriter [TCWrite ann] m,
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
    tClosure <- M.delete ident <$> getClosureType tAnn tBody
    pure (tBody, tClosure, subs)
  let lambdaType =
        substituteMany
          (filterSubstitutions subs)
          ( TFunc
              tAnn
              typedClosure
              realFrom
              (getExprAnnotation typedBody)
          )
  pure (ELambda lambdaType ident typedBody)
checkLambda other@(TUnknown {}) ident body = do
  -- unknown type - make a new unknown and keep smashing
  let tAnn = getTypeAnnotation other
  tyFrom <- getUnknown tAnn
  (typedBody, typedClosure, subs) <- withVar ident tyFrom $ do
    (tBody, subs) <- listen (infer body)
    tClosure <- M.delete ident <$> getClosureType tAnn tBody
    pure (tBody, tClosure, subs)
  let lambdaType =
        substituteMany
          (filterSubstitutions subs)
          ( TFunc
              tAnn
              typedClosure
              tyFrom
              (getExprAnnotation typedBody)
          )
  pure (ELambda lambdaType ident typedBody)
checkLambda other _ _ = throwError (TCExpectedFunction other)

inferLambda ::
  ( Ord ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m,
    MonadState (TCState ann) m,
    MonadWriter [TCWrite ann] m
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
    tClosure <- M.delete ident <$> getClosureType ann tBody
    pure (tBody, tClosure, subs)
  let lambdaType =
        substituteMany
          (filterSubstitutions subs)
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
    MonadWriter [TCWrite ann] m
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
    EApp ann fn arg ->
      inferApplication ann (Just typ) fn arg
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
      pure $ inferredExpr $> substituteMany (filterSubstitutions subs) realType
