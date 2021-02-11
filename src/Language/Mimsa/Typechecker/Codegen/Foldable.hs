{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.Foldable
  ( fold,
  )
where

import Control.Monad.Except
import Data.Foldable (foldl')
import Data.Text (Text)
import Language.Mimsa.Typechecker.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
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
          ( \(consName, fields) ->
              (,) consName <$> createMatch tyCon fVar fields
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
                        ( MyCaseMatch
                            mempty
                            (MyVar mempty tyName)
                            matches
                            Nothing
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

toFieldItemType :: TyCon -> Name -> Field -> CodegenM (Name, FieldItemType)
toFieldItemType tyName matchVar = \case
  VarName a ->
    if a == matchVar
      then pure (a, VariableField a)
      else pure (a, NoVariable)
  ConsName tyCon [VarName var] -> do
    varName <- nextName tyName
    if tyCon == tyName && var == matchVar
      then pure (varName, Recurse varName)
      else throwError "Can only recurse over self"
  _ -> throwError "Expected VarName"

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
  [Field] ->
  CodegenM (Expr Name ())
createMatch typeName matchVar fields = do
  fieldItems <- traverse (toFieldItemType typeName matchVar) fields
  let expr' =
        reconstructFields (snd <$> fieldItems)
  pure $
    foldr
      ( MyLambda mempty
      )
      expr'
      (fst <$> fieldItems)
