{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Smol.Core.Typecheck.Pattern
  ( checkPattern,
  )
where

import Control.Monad (when, zipWithM)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set.NonEmpty as NES
import Smol.Core.Helpers
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Simplify
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Types
import Smol.Core.Types

-- given the type of the expression in a pattern match,
-- check that the pattern makes sense with it
checkPattern ::
  ( Show ann,
    Eq ann,
    MonadError (TCError ann) m,
    MonadReader (TCEnv ann) m
  ) =>
  ResolvedType ann ->
  Pattern ResolvedDep ann ->
  m
    ( Pattern ResolvedDep (ResolvedType ann),
      Map (ResolvedDep Identifier) (ResolvedType ann)
    )
checkPattern checkTy checkPat = do
  tracePrettyM "checkPattern" (checkTy, checkPat)
  case (simplifyType checkTy, checkPat) of
    (TTuple _ tA tRest, PTuple ann pA pRest) | length tRest == length pRest -> do
      (patA, envA) <- checkPattern tA pA
      (patRest, envRest) <- neUnzip <$> neZipWithM checkPattern tRest pRest
      let ty = TTuple ann (getPatternAnnotation patA) (getPatternAnnotation <$> patRest)
          env = envA <> mconcat (NE.toList envRest)
      pure (PTuple ty patA patRest, env)
    (ty, PVar _ ident) -> do
      tracePrettyM "ident" (ident,ty)
      pure (PVar ty ident, M.singleton ident ty)
    (ty, PWildcard _) -> pure (PWildcard ty, mempty)
    (ty@(TLiteral _ (TLInt as)), PLiteral _ (PInt i))
      | NES.member i as ->
          pure (PLiteral ty (PInt i), mempty)
    (ty@(TLiteral _ (TLInt as)), PLiteral _ (PNat i))
      | NES.member (fromIntegral i) as ->
          pure (PLiteral ty (PNat i), mempty)
    (ty@(TLiteral _ tPrim), PLiteral _ pPrim)
      | tPrim == typeLiteralFromPrim pPrim ->
          pure (PLiteral ty pPrim, mempty)
    (ty@(TPrim _ TPBool), PLiteral _ (PBool b)) ->
      pure (PLiteral ty (PBool b), mempty)
    (ty@(TPrim _ TPNat), PLiteral _ (PNat b)) ->
      pure (PLiteral ty (PNat b), mempty)
    (ty@(TPrim _ TPInt), PLiteral _ (PInt b)) ->
      pure (PLiteral ty (PInt b), mempty)
    (ty@(TPrim _ TPString), PLiteral _ (PString s)) ->
      pure (PLiteral ty (PString s), mempty)
    (ty@(TArray _ arrSize tyArr), PArray ann items spread) -> do
      inferEverything <- traverse (checkPattern tyArr) items
      (inferSpread, env2) <- case spread of
        SpreadValue _ binder -> do
          let env = M.singleton binder ty
          pure
            ( SpreadValue ty binder,
              env
            )
        NoSpread -> pure (NoSpread, mempty)
        SpreadWildcard _ -> do
          pure (SpreadWildcard ty, mempty)

      let newEnv = mconcat (snd <$> inferEverything) <> env2

      pure
        ( PArray
            ( TArray
                ann
                arrSize
                tyArr
            )
            (fst <$> inferEverything)
            inferSpread,
          newEnv
        )
    (ty, PConstructor ann constructor args) -> do
      -- we don't check the constructor is valid yet
      let flattened = flattenConstructorType ty

      -- lookup the constructor itself (ie, `Just`, `Nothing`)
      (patTypeName, dtArgs, otherConstructors, consArgs) <-
        lookupConstructor constructor

      case flattened of
        Left _ -> do
          blah <- zipWithM checkPattern consArgs args
          tracePrettyM "blah" blah

          (patArgs, envArgs) <-
            unzip <$> zipWithM checkPattern consArgs args

          tracePrettyM "consArgs" consArgs
          tracePrettyM "args" args
          tracePrettyM "envArgs" envArgs
          tracePrettyM "patArgs" patArgs

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

          let pairs = zipWith (Substitution . SubId . LocalDefinition) dtArgs tyArgs
              resolvedArgs = substituteMany pairs <$> consArgs

          (patArgs, envArgs) <-
            unzip <$> zipWithM checkPattern resolvedArgs args -- tyArgs was consArgs

          tracePrettyM "envArgs" envArgs

          -- check number of args matches what constructor expects
          when
            (length patArgs /= length consArgs)
            $ throwError
              (TCConstructorArgumentMismatch constructor (length consArgs) (length patArgs))

          let constructorTy = dataTypeWithVars ann patTypeName tyArgs
          pure (PConstructor constructorTy constructor patArgs, mconcat envArgs)
    (otherTy, otherPat) -> throwError (TCPatternMismatch otherPat otherTy)
