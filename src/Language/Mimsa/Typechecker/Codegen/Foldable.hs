{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.Foldable
  ( fold,
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Prelude hiding (fmap)

type FoldableM = StateT (Map Name Int) (Either Text)

runFoldableM :: FoldableM a -> Either Text a
runFoldableM fn = case runStateT fn mempty of
  Right (a, _) -> pure a
  Left e -> Left e

fold :: DataType -> Either Text (Expr Name ())
fold = runFoldableM . fold_

-- | A newtype is a datatype with one constructor
-- | with one argument
fold_ ::
  DataType ->
  FoldableM (Expr Name ())
fold_ (DataType tyCon vars items) = do
  let tyName = tyConToName tyCon
  fVar <- getFoldableVar vars
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

getFoldableVar :: [Name] -> FoldableM Name
getFoldableVar names = case NE.nonEmpty names of
  Just neNames -> pure $ NE.last neNames
  _ -> throwError "Type should have at least one type variable"

newtype FieldItemType = VariableField Name

toFieldItemType :: TyCon -> Field -> FoldableM FieldItemType
toFieldItemType _typeName = \case
  VarName a -> pure (VariableField a)
  _ -> throwError "Expected VarName"

reconstructField :: Name -> FieldItemType -> Expr Name ()
reconstructField matchVar fieldItem =
  case fieldItem of
    VariableField varName ->
      if varName == matchVar
        then
          MyApp
            mempty
            (MyApp mempty (MyVar mempty "f") (MyVar mempty "total"))
            (MyVar mempty varName)
        else MyVar mempty "total"

createMatch ::
  TyCon ->
  Name ->
  [Field] ->
  FoldableM (Expr Name ())
createMatch typeName matchVar fields = do
  fieldItem <- case fields of
    [field] -> toFieldItemType typeName field
    _ -> throwError "Expected constructor with a single type variable"
  let expr' = reconstructField matchVar fieldItem
  case fieldItem of
    VariableField n -> pure $ MyLambda mempty n expr'

getMapItems :: Map k a -> Maybe (NE.NonEmpty (k, a))
getMapItems = NE.nonEmpty . M.toList
