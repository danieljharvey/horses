{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Codegen.ApplicativeApply
  ( applicativeApply,
  )
where

import Control.Applicative
import Control.Monad.Except
import Data.Coerce
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.CodegenError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Prelude hiding (fmap)

applicativeApply :: DataType -> Either CodegenError (Expr Name ())
applicativeApply = runCodegenM . applicativeApply_

fName :: TyCon -> Name
fName tyCon = Name (coerce (tyConToName tyCon) <> "F")

aName :: TyCon -> Name
aName tyCon = Name (coerce (tyConToName tyCon) <> "A")

-- | `pure` takes the rightmost var and places it in the functor context
-- | ie A -> m A
-- | If there are multiple constructors that match this it will fail
applicativeApply_ ::
  DataType ->
  CodegenM (Expr Name ())
applicativeApply_ (DataType tyCon vars items) = do
  matches <- createMatches tyCon vars items
  pure
    ( MyLambda
        mempty
        (Identifier mempty (fName tyCon))
        ( MyLambda
            mempty
            (Identifier mempty (aName tyCon))
            ( MyPatternMatch
                mempty
                (MyVar mempty Nothing (fName tyCon))
                (NE.toList matches)
            )
        )
    )

-- Do we care about this constructor?
containsVar :: Name -> [Type a] -> Bool
containsVar n fields =
  or (fieldContains <$> fields)
  where
    fieldContains field =
      case field of
        (MTVar _ (TVName a)) -> coerce a == n
        (MTFunction _ a b) -> fieldContains a || fieldContains b
        (MTPair _ a b) -> fieldContains a || fieldContains b
        (MTRecord _ items) -> or (fieldContains <$> items)
        mt -> case varsFromDataType mt of
          Just (_, as') -> or (fieldContains <$> as')
          _ -> False

createMatches ::
  TyCon ->
  [Name] ->
  Map TyCon [Type a] ->
  CodegenM (NonEmpty (Pattern Name (), Expr Name ()))
createMatches typeName vars items = do
  funcVar <- getFunctorVar vars
  constructors <- getMapItemsM items
  traverse
    ( \(k, as) ->
        if containsVar funcVar as
          then createMatch typeName funcVar items (k, as)
          else pure (noOpMatch k as)
    )
    constructors

-- | a case match that reconstructs the given expr untouched
noOpMatch :: TyCon -> [Type a] -> (Pattern Name (), Expr Name ())
noOpMatch tyCon fields =
  let numberedFields = (reverse $ zip ([1 ..] :: [Integer]) (reverse fields))
      mkFieldName i = Name ("a" <> T.pack (show i))
      patternExpr =
        foldl'
          ( \expr' (i, _field) ->
              let fieldName = mkFieldName i
               in MyApp mempty expr' (MyVar mempty Nothing fieldName)
          )
          (MyConstructor mempty tyCon)
          numberedFields
      pat = PConstructor mempty tyCon (PVar mempty . mkFieldName . fst <$> numberedFields)
   in (pat, patternExpr)

newtype FieldItemType = VariableField Name
  deriving stock (Eq, Ord)

toFieldItemType :: Type a -> CodegenM FieldItemType
toFieldItemType = \case
  MTVar _ (TVName a) -> pure (VariableField $ coerce a)
  _ -> throwError CouldNotFindVarsInType

toFieldItemTypeF ::
  Name ->
  Type a ->
  CodegenM FieldItemType
toFieldItemTypeF funcVar = \case
  MTVar _ (TVName a) ->
    if coerce a == funcVar
      then pure (VariableField "f")
      else pure (VariableField (coerce a))
  _ -> throwError CouldNotFindVarsInType

reconstructField :: Name -> FieldItemType -> Expr Name ()
reconstructField matchVar fieldItem =
  case fieldItem of
    VariableField varName ->
      if varName == matchVar
        then MyApp mempty (MyVar mempty Nothing "f") (MyVar mempty Nothing varName)
        else MyVar mempty Nothing varName

createInnerMatch ::
  Name ->
  TyCon ->
  [Type a] ->
  CodegenM (Pattern Name (), Expr Name ())
createInnerMatch matchVar tyCon fields = do
  regFields <-
    traverse toFieldItemType fields
  let withConsApp =
        foldl'
          ( \expr' fieldItem ->
              let reconstruct = reconstructField matchVar fieldItem
               in MyApp mempty expr' reconstruct
          )
          (MyConstructor mempty tyCon)
          regFields
  let pat = patternFromFieldItemType tyCon regFields
  pure
    ( pat,
      withConsApp
    )

-- we can't cope with two functor F values right now
multiFunctorCheck :: [FieldItemType] -> CodegenM ()
multiFunctorCheck items =
  if length items == S.size (S.fromList items)
    then pure ()
    else
      throwError
        MultipleFunctorVariablesInApplicativeArg

patternFromFieldItemType :: TyCon -> [FieldItemType] -> Pattern Name ()
patternFromFieldItemType tyCon fields =
  PConstructor mempty tyCon (toPat <$> fields)
  where
    toPat (VariableField a) = PVar mempty a

createMatch ::
  TyCon ->
  Name ->
  Map TyCon [Type a] ->
  (TyCon, [Type a]) ->
  CodegenM (Pattern Name (), Expr Name ())
createMatch typeName funcVar items (tyCon, fields) = do
  regFields <- traverse (toFieldItemTypeF funcVar) fields
  multiFunctorCheck regFields
  constructors <- getMapItemsM items
  matches <-
    traverse
      ( \(k, as) ->
          if containsVar funcVar as
            then createInnerMatch funcVar k as
            else pure (noOpMatch k as)
      )
      constructors

  pure
    ( patternFromFieldItemType tyCon regFields,
      MyPatternMatch
        mempty
        (MyVar mempty Nothing (aName typeName))
        (NE.toList matches)
    )
