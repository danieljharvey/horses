{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.Foldable
  ( fold,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldl')
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

getFoldableVar :: [Name] -> FoldableM Name
getFoldableVar names = case NE.nonEmpty names of
  Just neNames -> pure $ NE.last neNames
  _ -> throwError "Type should have at least one type variable"

data FieldItemType
  = VariableField Name
  | NoVariable

toFieldItemType :: Name -> Field -> FoldableM (Name, FieldItemType)
toFieldItemType matchVar = \case
  VarName a ->
    if a == matchVar
      then pure (a, VariableField a)
      else pure (a, NoVariable)
  _ -> throwError "Expected VarName"

reconstructFields :: [FieldItemType] -> Expr Name ()
reconstructFields =
  foldl'
    ( \expr' -> \case
        NoVariable -> expr'
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
  FoldableM (Expr Name ())
createMatch _typeName matchVar fields = do
  fieldItems <- traverse (toFieldItemType matchVar) fields
  let expr' =
        reconstructFields (snd <$> fieldItems)
  pure $
    foldr
      ( MyLambda mempty
      )
      expr'
      (fst <$> fieldItems)

getMapItems :: Map k a -> Maybe (NE.NonEmpty (k, a))
getMapItems = NE.nonEmpty . M.toList
