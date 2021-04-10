{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.ApplicativeApply
  ( applicativeApply,
  )
where

import Control.Applicative
import Control.Monad.Except
import Data.Coerce
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Typechecker.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Prelude hiding (fmap)

applicativeApply :: DataType () -> Either Text (Expr Name ())
applicativeApply = runCodegenM . applicativeApply_

fName :: TyCon -> Name
fName tyCon = Name (coerce (tyConToName tyCon) <> "F")

aName :: TyCon -> Name
aName tyCon = Name (coerce (tyConToName tyCon) <> "A")

-- | `pure` takes the rightmost var and places it in the functor context
-- | ie A -> m A
-- | If there are multiple constructors that match this it will fail
applicativeApply_ ::
  DataType () ->
  CodegenM (Expr Name ())
applicativeApply_ (DataType tyCon vars items) = do
  matches <- createMatches tyCon vars items
  pure
    ( MyLambda
        mempty
        (fName tyCon)
        ( MyLambda
            mempty
            (aName tyCon)
            ( MyCaseMatch
                mempty
                (MyVar mempty (fName tyCon))
                matches
                Nothing
            )
        )
    )

-- Do we care about this constructor?
containsVar :: Name -> [Type ()] -> Bool
containsVar n fields =
  or (fieldContains <$> fields)
  where
    fieldContains field =
      case field of
        (MTVar _ (TVName a)) -> coerce a == n
        (MTData _ _ as') -> or (fieldContains <$> as')
        (MTFunction _ a b) -> fieldContains a || fieldContains b
        (MTPair _ a b) -> fieldContains a || fieldContains b
        (MTRecord _ items) -> or (fieldContains <$> items)
        _ -> False

createMatches ::
  TyCon ->
  [Name] ->
  Map TyCon [Type ()] ->
  CodegenM (NonEmpty (TyCon, Expr Name ()))
createMatches typeName vars items = do
  funcVar <- getFunctorVar vars
  constructors <- getMapItemsM items
  traverse
    ( \(k, as) ->
        if containsVar funcVar as
          then createMatch typeName funcVar items (k, as)
          else pure (k, noOpMatch k as)
    )
    constructors

-- | a case match that reconstructs the given expr untouched
noOpMatch :: TyCon -> [Type ()] -> Expr Name ()
noOpMatch tyCon fields =
  foldl'
    ( \expr' (i, _field) ->
        let fieldName = Name ("a" <> T.pack (show i))
         in MyLambda mempty fieldName (MyConsApp mempty expr' (MyVar mempty fieldName))
    )
    (MyConstructor mempty tyCon)
    (reverse $ zip ([1 ..] :: [Integer]) (reverse fields))

newtype FieldItemType = VariableField Name
  deriving (Eq, Ord)

toFieldItemType :: Type () -> CodegenM FieldItemType
toFieldItemType = \case
  MTVar _ (TVName a) -> pure (VariableField $ coerce a)
  _ -> throwError "Expected VarName"

toFieldItemTypeF ::
  Name ->
  Type () ->
  CodegenM FieldItemType
toFieldItemTypeF funcVar = \case
  MTVar _ (TVName a) ->
    if coerce a == funcVar
      then pure (VariableField "f")
      else pure (VariableField (coerce a))
  _ -> throwError "Expected VarName"

reconstructField :: Name -> FieldItemType -> Expr Name ()
reconstructField matchVar fieldItem =
  case fieldItem of
    VariableField varName ->
      if varName == matchVar
        then MyApp mempty (MyVar mempty "f") (MyVar mempty varName)
        else MyVar mempty varName

createInnerMatch ::
  Name ->
  TyCon ->
  [Type ()] ->
  CodegenM (Expr Name ())
createInnerMatch matchVar tyCon fields = do
  regFields <-
    traverse toFieldItemType fields
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
      )
      withConsApp
      regFields

-- we can't cope with two functor F values right now
multiFunctorCheck :: [FieldItemType] -> CodegenM ()
multiFunctorCheck items =
  if length items == S.size (S.fromList items)
    then pure ()
    else
      throwError
        "Multiple functor variables in first applicative argument"

createMatch ::
  TyCon ->
  Name ->
  Map
    TyCon
    [Type ()] ->
  (TyCon, [Type ()]) ->
  CodegenM (TyCon, Expr Name ())
createMatch typeName funcVar items (tyCon, fields) = do
  regFields <- traverse (toFieldItemTypeF funcVar) fields
  multiFunctorCheck regFields
  let lambdas expr' =
        foldr
          ( \case
              VariableField n -> MyLambda mempty n
          )
          expr'
          regFields
  constructors <- getMapItemsM items
  matches <-
    traverse
      ( \(k, as) ->
          if containsVar funcVar as
            then do
              innerMatch <- createInnerMatch funcVar k as
              pure (k, innerMatch)
            else pure (k, noOpMatch k as)
      )
      constructors

  pure
    ( tyCon,
      lambdas
        ( MyCaseMatch
            mempty
            (MyVar mempty (aName typeName))
            matches
            Nothing
        )
    )
