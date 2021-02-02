{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.Functor
  ( functorMap,
  )
where

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
functorMap :: DataType -> Either Text (Expr Name ())
functorMap (DataType tyCon vars items) = do
  let tyName = tyConToName tyCon
  fVar <- getFunctorVar vars
  case getMapItems items of
    Nothing -> Left "Type should have at least one constructor"
    Just constructors -> do
      matches <-
        traverse
          ( \(consName, fields) ->
              (,) consName <$> createMatch fVar consName fields
          )
          constructors
      pure
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

getFunctorVar :: [Name] -> Either Text Name
getFunctorVar names = case NE.nonEmpty names of
  Just neNames -> Right $ NE.last neNames
  _ -> Left "Type should have at least one type variable"

createMatch :: Name -> TyCon -> [Field] -> Either Text (Expr Name ())
createMatch matchVar tyCon fields = do
  regFields <-
    traverse
      ( \case
          VarName a -> Right a
          _ -> Left "Expected VarName"
      )
      fields
  let withConsApp =
        foldl'
          ( \expr' varName ->
              let var =
                    if varName == matchVar
                      then MyApp mempty (MyVar mempty "f") (MyVar mempty varName)
                      else MyVar mempty varName
               in MyConsApp mempty expr' var
          )
          (MyConstructor mempty tyCon)
          regFields
      expression =
        foldr
          ( MyLambda mempty
          )
          withConsApp
          regFields
  pure expression

getMapItems :: Map k a -> Maybe (NE.NonEmpty (k, a))
getMapItems = NE.nonEmpty . M.toList
