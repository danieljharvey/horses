{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Modules.Check (checkModule, exprAndTypeFromParts) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Coerce
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Module
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Typechecker

-- | This is where we load a file and check that it is "OK" as such
--  so far this entails:
--  1. parsing it
--  2. ordering things
--  3. typechecking everything
--
--  so far the features in modules are
--  1. definitions of values
--  2. types of values
--  3. definitions of datatypes
--
--  soon there will also need to be
--  1. infix definitions
--  2. imports
--  3. exports
checkModule :: Text -> Either (Error Annotation) (Module Annotation)
checkModule input = do
  moduleItems <- first (ParseError input) (parseModule input)
  first ModuleErr (moduleFromModuleParts moduleItems)

-- type Deps a = (a, [a])

moduleFromModuleParts :: (Monoid ann) => [ModuleItem ann] -> Either ModuleError (Module ann)
moduleFromModuleParts parts =
  let addPart part = \case
        Left e -> Left e
        Right mod' -> case part of
          ModuleExpression name bits expr ->
            case M.lookup name (moExpressions mod') of
              Just _ -> throwError (DuplicateDefinition name)
              Nothing ->
                let (exp', mt) = exprAndTypeFromParts bits expr
                 in pure $
                      mod'
                        { moExpressions =
                            M.singleton name (mt, exp') <> moExpressions mod'
                        }
          ModuleDataType dt@(DataType tyCon _ _) ->
            let typeName = coerce tyCon
             in case M.lookup typeName (moDataTypes mod') of
                  Just _ -> throwError (DuplicateTypeName typeName)
                  Nothing ->
                    pure $
                      mod'
                        { moDataTypes =
                            M.singleton typeName dt
                              <> moDataTypes mod'
                        }
   in foldr addPart (Right mempty) parts

-- given the bits of things, make a coherent type and expression
exprAndTypeFromParts ::
  (Monoid ann) =>
  [DefPart ann] ->
  Expr Name ann ->
  (Expr Name ann, Maybe (Type ann))
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
   in (expr', mt)
