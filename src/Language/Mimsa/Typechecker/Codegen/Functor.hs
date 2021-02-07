{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Typechecker.Codegen.Functor
  ( functorMap,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Prelude hiding (fmap)

type FunctorM = StateT (Map Name Int) (Either Text)

runFunctorM :: FunctorM a -> Either Text a
runFunctorM fn = case runStateT fn mempty of
  Right (a, _) -> pure a
  Left e -> Left e

functorMap :: DataType -> Either Text (Expr Name ())
functorMap = runFunctorM . functorMap_

-- | A newtype is a datatype with one constructor
-- | with one argument
functorMap_ ::
  DataType ->
  FunctorM (Expr Name ())
functorMap_ (DataType tyCon vars items) = do
  let tyName = tyConToName tyCon
  fVar <- getFunctorVar vars
  case getMapItems items of
    Nothing -> throwError "Type should have at least one constructor"
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

getFunctorVar :: (MonadError Text m) => [Name] -> m Name
getFunctorVar names = case NE.nonEmpty names of
  Just neNames -> pure $ NE.last neNames
  _ -> throwError "Type should have at least one type variable"

data FieldItemType
  = VariableField Name
  | RecurseField Name

toFieldItemType :: TyCon -> Field -> FunctorM FieldItemType
toFieldItemType typeName = \case
  VarName a -> pure (VariableField a)
  ConsName fieldConsName _fields
    | fieldConsName == typeName ->
      RecurseField <$> nextName typeName
  _ -> throwError "Expected VarName"

-- | given a type constructor, give me a new unique name for it
nextName :: TyCon -> FunctorM Name
nextName tyCon = do
  let base = tyConToName tyCon
  vars <- get
  case M.lookup base vars of
    Nothing -> do
      modify (M.singleton base 1 <>)
      pure $ base <> Name "1"
    Just as -> do
      modify (M.adjust (+ 1) base)
      pure $ base <> Name (prettyPrint (as + 1))

-- | A match is one item in a case match, that will deconstruct and then
-- rebuild whatever is inside, mapping over `f` as it does.
-- TODO: allow the `f` to be configurable and to allow multiple ones so we can
-- implement bifunctor with the same code
createMatch ::
  TyCon ->
  Name ->
  TyCon ->
  [Field] ->
  FunctorM (Expr Name ())
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
