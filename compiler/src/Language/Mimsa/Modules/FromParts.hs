{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Modules.FromParts (addModulePart, moduleFromModuleParts, exprAndTypeFromParts) where

import Control.Monad.Except
import Data.Coerce
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Parser.Module
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Tests
import Language.Mimsa.Types.Typechecker

moduleFromModuleParts ::
  ( MonadError (Error Annotation) m,
    Monoid ann
  ) =>
  Map ModuleHash (Module Annotation) ->
  [ModuleItem ann] ->
  m (Module ann)
moduleFromModuleParts modules parts =
  let addPart part output = do
        mod' <- output
        addModulePart modules part mod'
   in foldr addPart (pure mempty) parts

addModulePart ::
  (MonadError (Error Annotation) m, Monoid ann) =>
  Map ModuleHash (Module Annotation) ->
  ModuleItem ann ->
  Module ann ->
  m (Module ann)
addModulePart modules part mod' =
  case part of
    ModuleExpression name bits expr -> do
      errorIfExpressionAlreadyDefined mod' (DIName name)
      exp' <- exprAndTypeFromParts (DIName name) bits expr
      pure $
        mod'
          { moExpressions =
              M.singleton (DIName name) exp' <> moExpressions mod'
          }
    ModuleDataType dt@(DataType tyCon _ _) -> do
      let typeName = coerce tyCon
      checkDataType mod' dt
      pure $
        mod'
          { moDataTypes =
              M.singleton typeName dt
                <> moDataTypes mod'
          }
    ModuleInfix infixOp expr -> do
      errorIfExpressionAlreadyDefined mod' (DIInfix infixOp)
      pure $
        mod'
          { moExpressions =
              M.singleton (DIInfix infixOp) expr
                <> moExpressions mod'
          }
    ModuleTest testName expr -> do
      errorIfExpressionAlreadyDefined mod' (DITest testName)
      when
        (testName == TestName "")
        ( throwError (ModuleErr (EmptyTestName $ void expr))
        )

      pure $
        mod'
          { moExpressions =
              M.singleton (DITest testName) expr
                <> moExpressions mod'
          }
    ModuleExport modItem -> do
      -- get whatever is inside
      innerModule <- addModulePart modules modItem mod'
      -- get the keys, add them to exports
      let defExports = case modItem of
            ModuleExpression name _ _ -> S.singleton (DIName name)
            ModuleInfix infixOp _ -> S.singleton (DIInfix infixOp)
            _ -> mempty
      let typeExports = case modItem of
            ModuleDataType (DataType tn _ _) -> S.singleton (coerce tn)
            _ -> mempty
      pure $
        innerModule
          { moExpressionExports =
              defExports <> moExpressionExports innerModule,
            moDataTypeExports =
              typeExports <> moDataTypeExports innerModule
          }
    ModuleImport (ImportNamedFromHash mHash mName) ->
      pure $ mod' {moNamedImports = M.singleton mName mHash <> moNamedImports mod'}
    ModuleImport (ImportAllFromHash mHash) -> do
      importMod <- lookupModule modules mHash
      let defImports =
            M.fromList
              . fmap (,mHash)
              . S.toList
              . moExpressionExports
              $ importMod

      -- explode if these are defined already
      _ <-
        M.traverseWithKey
          (errorIfImportAlreadyDefined mod')
          defImports

      let typeImports =
            M.fromList
              . fmap (,mHash)
              . S.toList
              . moDataTypeExports
              $ importMod

      -- explode if these types are defined already
      _ <-
        M.traverseWithKey
          (errorIfTypeImportAlreadyDefined mod')
          typeImports

      pure $
        mod'
          { moExpressionImports =
              defImports
                <> moExpressionImports mod',
            moDataTypeImports =
              typeImports
                <> moDataTypeImports mod'
          }

addAnnotation :: Maybe (Type ann) -> Expr Name ann -> Expr Name ann
addAnnotation mt expr =
  -- add type annotation to expression
  case mt of
    Just typeAnnotation ->
      MyAnnotation
        (getAnnotationForType typeAnnotation)
        typeAnnotation
        expr
    _ -> expr

includesExplicitTypes :: [DefPart ann] -> Bool
includesExplicitTypes =
  any
    ( \case
        (DefArg _) -> False
        _ -> True
    )

includesReturnType :: [DefPart ann] -> Bool
includesReturnType =
  any
    ( \case
        (DefType _) -> True
        _ -> False
    )

-- given the bits of things, make a coherent type and expression
-- 1) check we have any type annotations
-- 2) if so - ensure we have a full set (error if not) and create annotation
-- 3) if not, just return expr
exprAndTypeFromParts ::
  (MonadError (Error Annotation) m, Monoid ann) =>
  DefIdentifier ->
  [DefPart ann] ->
  Expr Name ann ->
  m (Expr Name ann)
exprAndTypeFromParts def parts expr = do
  let expr' =
        foldr
          ( \part rest -> case part of
              (DefArg ident) -> MyLambda mempty ident rest
              (DefTypedArg ident _) -> MyLambda mempty ident rest
              (DefType _) -> rest
          )
          expr
          parts
  -- if we only have un-typed args, don't bother, we only want them as
  -- placeholders
  if not (includesExplicitTypes parts)
    then pure expr'
    else do
      if includesReturnType parts
        then pure ()
        else throwError (ModuleErr (DefMissingReturnType def))
      mt <-
        foldr
          ( \part mRest -> do
              rest <- mRest
              case part of
                (DefArg (Identifier _ name)) ->
                  throwError (ModuleErr (DefMissingTypeAnnotation def name))
                (DefTypedArg _ thisMt) -> pure $ case rest of
                  Just rest' ->
                    Just
                      (MTFunction mempty thisMt rest')
                  _ -> Just thisMt
                (DefType thisMt) -> pure $ case rest of
                  Just rest' ->
                    Just
                      (MTFunction mempty rest' thisMt)
                  _ -> Just thisMt
          )
          (pure Nothing)
          parts
      pure $ addAnnotation mt expr'
