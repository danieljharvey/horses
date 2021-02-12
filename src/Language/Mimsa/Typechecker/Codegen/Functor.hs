{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.Functor
  ( functorMap,
  )
where

import Control.Monad.Except
import Data.Foldable (foldl')
import Data.Semigroup
import Data.Text (Text)
import Language.Mimsa.Typechecker.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Prelude hiding (fmap)

functorMap :: DataType -> Either Text (Expr Name ())
functorMap = runCodegenM . functorMap_

-- | A newtype is a datatype with one constructor
-- | with one argument
functorMap_ ::
  DataType ->
  CodegenM (Expr Name ())
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

data FieldItemType
  = VariableField Name
  | RecurseField Name
  | Func2 Name Name Name

toFieldItemType :: TyCon -> Field -> CodegenM FieldItemType
toFieldItemType typeName = \case
  VarName a -> pure (VariableField a)
  ConsName fieldConsName _fields
    | fieldConsName == typeName ->
      RecurseField <$> nextName typeName
  TNFunc (VarName a) (VarName b) ->
    pure $ Func2 (a <> "to" <> b) a b
  _ -> throwError "Expected VarName"

reconstructField :: Name -> FieldItemType -> Expr Name ()
reconstructField matchVar fieldItem =
  case fieldItem of
    VariableField varName ->
      if varName == matchVar
        then MyApp mempty (MyVar mempty "f") (MyVar mempty varName)
        else MyVar mempty varName
    RecurseField restVar ->
      MyApp
        mempty
        ( MyApp
            mempty
            (MyVar mempty "fmap")
            (MyVar mempty "f")
        )
        (MyVar mempty restVar)
    Func2 fromF from to ->
      if to == matchVar
        then
          MyLambda
            mempty
            from
            ( MyApp
                mempty
                (MyVar mempty "f")
                ( MyApp
                    mempty
                    (MyVar mempty fromF)
                    (MyVar mempty from)
                )
            )
        else
          MyLambda
            mempty
            from
            ( MyApp
                mempty
                (MyVar mempty fromF)
                (MyVar mempty from)
            )

-- | A match is one item in a case match, that will deconstruct and then
-- rebuild whatever is inside, mapping over `f` as it does.
-- TODO: allow the `f` to be configurable and to allow multiple ones so we can
-- implement bifunctor with the same code
createMatch ::
  TyCon ->
  Name ->
  TyCon ->
  [Field] ->
  CodegenM (Expr Name ())
createMatch typeName matchVar tyCon fields = do
  regFields <-
    traverse (toFieldItemType typeName) fields
  let withConsApp =
        foldl'
          ( \expr' fieldItem ->
              let reconstruct = reconstructField matchVar fieldItem
               in MyConsApp mempty expr' reconstruct
          )
          (MyConstructor mempty tyCon)
          regFields
  pure $
    foldr
      ( \case
          VariableField n -> MyLambda mempty n
          RecurseField restVar -> MyLambda mempty restVar
          Func2 fromF _ _ -> MyLambda mempty fromF
      )
      withConsApp
      regFields
