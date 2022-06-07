{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Modules.FromParts (moduleFromModuleParts, exprAndTypeFromParts) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Coerce
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Parser.Module
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Typechecker

moduleFromModuleParts ::
  (Monoid ann) =>
  [ModuleItem ann] ->
  CheckM (Module ann)
moduleFromModuleParts parts =
  let addPart part output = do
        mod' <- output
        case part of
          ModuleExport modItem -> do
            -- get whatever is inside
            innerModule <- addPart modItem output
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
          ModuleImport (ImportNamedFromHash mHash mName) ->
            pure $ mod' {moNamedImports = M.singleton mName mHash <> moNamedImports mod'}
          ModuleImport (ImportAllFromHash mHash) -> do
            modules <- asks ceModules
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
   in foldr addPart (pure mempty) parts

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
  (Monoid ann) =>
  DefIdentifier ->
  [DefPart ann] ->
  Expr Name ann ->
  CheckM (Expr Name ann)
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
