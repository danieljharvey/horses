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
import Data.Text (Text)
import Language.Mimsa.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Prelude hiding (fmap)

fold :: DataType -> Either Text (Expr Name ())
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
    Nothing -> throwError "Type should have at least one constructor"
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
            "fold"
            ( MyLambda
                mempty
                "f"
                ( MyLambda
                    mempty
                    "total"
                    ( MyLambda
                        mempty
                        tyName
                        ( MyPatternMatch
                            mempty
                            (MyVar mempty tyName)
                            (NE.toList matches)
                        )
                    )
                )
            )
            (MyVar mempty "fold")
        )

data FieldItemType
  = VariableField Name
  | Recurse Name
  | NoVariable

toFieldItemType ::
  TyCon ->
  Name ->
  Type () ->
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
        else throwError "Can only recurse over self"
    _ -> throwError "Expected VarName"

patternFromFieldItemType :: TyCon -> [Name] -> Pattern Name ()
patternFromFieldItemType tyCon names =
  PConstructor mempty tyCon (patForField <$> names)
  where
    patForField a = PVar mempty a

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
                          "fold"
                      )
                      (MyVar mempty "f")
                  )
                  expr'
              )
              (MyVar mempty tyName)
          )
        VariableField a ->
          ( MyApp
              mempty
              (MyApp mempty (MyVar mempty "f") expr')
              (MyVar mempty a)
          )
    )
    (MyVar mempty "total")

createMatch ::
  TyCon ->
  Name ->
  TyCon ->
  [Type ()] ->
  CodegenM (Pattern Name (), Expr Name ())
createMatch typeName matchVar tyCon fields = do
  fieldItems <- traverse (toFieldItemType typeName matchVar) fields
  let expr' =
        reconstructFields (snd <$> fieldItems)
  pure
    ( patternFromFieldItemType tyCon (fst <$> fieldItems),
      expr'
    )
