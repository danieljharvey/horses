{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Codegen.Foldable
  ( fold,
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import Language.Mimsa.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.CodegenError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Prelude hiding (fmap)

fold :: DataType -> Either CodegenError (Expr Name ())
fold = runCodegenM . fold_

-- | A newtype is a datatype with one constructor
-- | with one argument
fold_ ::
  DataType ->
  CodegenM (Expr Name ())
fold_ (DataType tyCon vars items) = do
  let tyName = tyConToName tyCon
  fVar <- getFunctorVar vars
  case getMapItems items of
    Nothing -> throwError NoConstructorMatches
    Just constructors -> do
      matches <-
        traverse
          ( uncurry
              ( createMatch tyCon fVar
              )
          )
          constructors
      pure
        ( MyLet
            mempty
            (Identifier mempty "fold")
            ( MyLambda
                mempty
                (Identifier mempty "f")
                ( MyLambda
                    mempty
                    (Identifier mempty "total")
                    ( MyLambda
                        mempty
                        (Identifier mempty tyName)
                        ( MyPatternMatch
                            mempty
                            (MyVar mempty Nothing tyName)
                            (NE.toList matches)
                        )
                    )
                )
            )
            (MyVar mempty Nothing "fold")
        )

data FieldItemType
  = VariableField Name
  | Recurse Name
  | NoVariable

toFieldItemType ::
  TyCon ->
  Name ->
  Type a ->
  CodegenM (Name, FieldItemType)
toFieldItemType tyName matchVar = \case
  MTVar _ (TVName a) ->
    if coerce a == matchVar
      then do
        name <- nextName (coerce a)
        pure (name, VariableField name)
      else pure (coerce a, NoVariable)
  mt -> case varsFromDataType mt of
    Just (tyCon, [MTVar _ (TVName var)]) -> do
      varName <- nextName tyName
      if tyCon == tyName && coerce var == matchVar
        then pure (varName, Recurse varName)
        else throwError RecursingOverAnotherType
    _ -> throwError CouldNotFindVarsInType

patternFromFieldItemType :: TyCon -> [Name] -> Pattern Name ()
patternFromFieldItemType tyCon names =
  PConstructor mempty Nothing tyCon (patForField <$> names)
  where
    patForField = PVar mempty

reconstructFields :: [FieldItemType] -> Expr Name ()
reconstructFields =
  foldl'
    ( \expr' -> \case
        NoVariable -> expr'
        Recurse tyName ->
          ( MyApp
              mempty
              ( MyApp
                  mempty
                  ( MyApp
                      mempty
                      ( MyVar
                          mempty
                          Nothing
                          "fold"
                      )
                      (MyVar mempty Nothing "f")
                  )
                  expr'
              )
              (MyVar mempty Nothing tyName)
          )
        VariableField a ->
          ( MyApp
              mempty
              (MyApp mempty (MyVar mempty Nothing "f") expr')
              (MyVar mempty Nothing a)
          )
    )
    (MyVar mempty Nothing "total")

createMatch ::
  TyCon ->
  Name ->
  TyCon ->
  [Type a] ->
  CodegenM (Pattern Name (), Expr Name ())
createMatch typeName matchVar tyCon fields = do
  fieldItems <- traverse (toFieldItemType typeName matchVar) fields
  let expr' =
        reconstructFields (snd <$> fieldItems)
  pure
    ( patternFromFieldItemType tyCon (fst <$> fieldItems),
      expr'
    )
