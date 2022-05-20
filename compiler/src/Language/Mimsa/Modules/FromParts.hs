{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Typechecker

lookupModule :: ModuleHash -> CheckM (Module Annotation)
lookupModule modHash = do
  mods <- asks ceModules
  case M.lookup modHash mods of
    Just foundModule -> pure foundModule
    _ -> throwError (ModuleErr (MissingModule modHash))

errorIfExpressionAlreadyDefined :: Module ann -> Name -> CheckM ()
errorIfExpressionAlreadyDefined mod' name =
  if M.member name (moExpressions mod')
    || M.member name (moExpressionImports mod')
    then throwError (ModuleErr $ DuplicateDefinition name)
    else pure ()

errorIfTypeAlreadyDefined :: Module ann -> TypeName -> CheckM ()
errorIfTypeAlreadyDefined mod' typeName =
  if M.member typeName (moDataTypes mod')
    || M.member typeName (moDataTypeImports mod')
    then throwError (ModuleErr $ DuplicateTypeName typeName)
    else pure ()

errorIfImportAlreadyDefined :: Module ann -> Name -> ModuleHash -> CheckM ()
errorIfImportAlreadyDefined mod' name moduleHash =
  if M.member name (moExpressions mod')
    || M.member name (moExpressionImports mod')
    then throwError (ModuleErr $ DefinitionConflictsWithImport name moduleHash)
    else pure ()

errorIfTypeImportAlreadyDefined :: Module ann -> TypeName -> ModuleHash -> CheckM ()
errorIfTypeImportAlreadyDefined mod' typeName moduleHash =
  if M.member typeName (moDataTypes mod')
    || M.member typeName (moDataTypeImports mod')
    then throwError (ModuleErr $ TypeConflictsWithImport typeName moduleHash)
    else pure ()

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
            pure $
              innerModule
                { moExpressionExports =
                    M.keysSet (moExpressions innerModule),
                  moDataTypeExports =
                    M.keysSet (moDataTypes innerModule)
                }
          ModuleExpression name bits expr -> do
            errorIfExpressionAlreadyDefined mod' name
            let exp' = exprAndTypeFromParts bits expr
            pure $
              mod'
                { moExpressions =
                    M.singleton name exp' <> moExpressions mod'
                }
          ModuleDataType dt@(DataType tyCon _ _) -> do
            let typeName = coerce tyCon
            errorIfTypeAlreadyDefined mod' typeName
            pure $
              mod'
                { moDataTypes =
                    M.singleton typeName dt
                      <> moDataTypes mod'
                }
          ModuleInfix infixOp expr -> do
            -- TODO: error on dupes
            pure $
              mod'
                { moInfixes = M.singleton infixOp expr <> moInfixes mod'
                }
          ModuleImport (ImportAllFromHash mHash) -> do
            importMod <- lookupModule mHash
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

-- given the bits of things, make a coherent type and expression
exprAndTypeFromParts ::
  (Monoid ann) =>
  [DefPart ann] ->
  Expr Name ann ->
  Expr Name ann
exprAndTypeFromParts parts expr =
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
      filteredParts =
        let includesExplicitTypes =
              any
                ( \case
                    (DefArg _) -> False
                    _ -> True
                )
                parts
            includesReturnType =
              any
                ( \case
                    (DefType _) -> True
                    _ -> False
                )
                parts
         in if includesExplicitTypes
              then
                if includesReturnType
                  then parts
                  else parts <> [DefType (MTVar mempty (TVName "returnType"))]
              else mempty
      mt =
        foldr
          ( \part rest -> case part of
              (DefArg (Identifier _ name)) -> case rest of
                Just rest' ->
                  Just
                    ( MTFunction
                        mempty
                        (MTVar mempty (TVName (coerce name)))
                        rest'
                    )
                Nothing ->
                  Just
                    ( MTVar
                        mempty
                        (TVName (coerce name))
                    )
              (DefTypedArg _ thisMt) -> case rest of
                Just rest' ->
                  Just
                    (MTFunction mempty thisMt rest')
                _ -> Just thisMt
              (DefType thisMt) -> case rest of
                Just rest' ->
                  Just
                    (MTFunction mempty rest' thisMt)
                _ -> Just thisMt
          )
          Nothing
          filteredParts
   in addAnnotation mt expr'
