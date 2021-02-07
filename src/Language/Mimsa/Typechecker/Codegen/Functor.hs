{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.Functor
  ( functorMap,
  )
where

import Data.Coerce
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Prelude hiding (fmap)

-- | A newtype is a datatype with one constructor
-- | with one argument
functorMap ::
  DataType ->
  Either Text (Expr Name ())
functorMap (DataType tyCon vars items) = do
  let tyName = tyConToName tyCon
  fVar <- getFunctorVar vars
  case getMapItems items of
    Nothing -> Left "Type should have at least one constructor"
    Just constructors -> do
      matches <-
        traverse
          ( \(consName, fields) ->
              (,) consName <$> createMatch tyCon fVar consName fields
          )
          constructors
      pure
        ( MyLet
            mempty
            "fmap"
            ( MyLambda
                mempty
                "f"
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
            (MyVar mempty "fmap")
        )

getFunctorVar :: [Name] -> Either Text Name
getFunctorVar names = case NE.nonEmpty names of
  Just neNames -> Right $ NE.last neNames
  _ -> Left "Type should have at least one type variable"

data FieldItemType
  = VariableField Name
  | RecurseField Name

toFieldItemType :: TyCon -> Field -> Either Text FieldItemType
toFieldItemType typeName = \case
  VarName a -> Right (VariableField a)
  ConsName fieldConsName _fields
    | fieldConsName == typeName ->
      Right (RecurseField (restName typeName))
  _ -> Left "Expected VarName"

restName :: TyCon -> Name
restName tyCon = "rest" <> coerce tyCon

createMatch ::
  TyCon ->
  Name ->
  TyCon ->
  [Field] ->
  Either Text (Expr Name ())
createMatch typeName matchVar tyCon fields = do
  regFields <-
    traverse (toFieldItemType typeName) fields
  let withConsApp =
        foldl'
          ( \expr' field -> case field of
              VariableField varName ->
                let var =
                      if varName == matchVar
                        then MyApp mempty (MyVar mempty "f") (MyVar mempty varName)
                        else MyVar mempty varName
                 in MyConsApp mempty expr' var
              RecurseField restVar ->
                let var =
                      MyApp
                        mempty
                        ( MyApp
                            mempty
                            (MyVar mempty "fmap")
                            (MyVar mempty "f")
                        )
                        (MyVar mempty restVar)
                 in MyConsApp mempty expr' var
          )
          (MyConstructor mempty tyCon)
          regFields
  pure $
    foldr
      ( \field expr' -> case field of
          VariableField n -> MyLambda mempty n expr'
          RecurseField restVar -> MyLambda mempty restVar expr'
      )
      withConsApp
      regFields

getMapItems :: Map k a -> Maybe (NE.NonEmpty (k, a))
getMapItems = NE.nonEmpty . M.toList
