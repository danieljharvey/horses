{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.ApplicativeApply
  ( applicativeApply,
  )
where

import Control.Applicative
import Data.Coerce
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Typechecker.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Prelude hiding (fmap)

applicativeApply :: DataType -> Either Text (Expr Name ())
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
containsVar :: Name -> [Field] -> Bool
containsVar n fields =
  or (fieldContains <$> fields)
  where
    fieldContains field =
      case field of
        (VarName a) -> a == n
        (ConsName _ as') -> or (fieldContains <$> as')
        (TNFunc a b) -> fieldContains a || fieldContains b

createMatches ::
  TyCon ->
  [Name] ->
  Map TyCon [Field] ->
  CodegenM (NonEmpty (TyCon, Expr Name ()))
createMatches typeName vars items = do
  funcVar <- getFunctorVar vars
  constructors <- getMapItemsM items
  pure $
    ( \(k, as) ->
        if containsVar funcVar as
          then createMatch typeName funcVar items (k, as)
          else (k, noOpMatch k as)
    )
      <$> constructors

-- | a case match that reconstructs the given expr untouched
noOpMatch :: TyCon -> [Field] -> Expr Name ()
noOpMatch tyCon fields =
  foldl'
    ( \expr' (i, _field) ->
        let fieldName = Name ("a" <> T.pack (show i))
         in MyLambda mempty fieldName (MyConsApp mempty expr' (MyVar mempty fieldName))
    )
    (MyConstructor mempty tyCon)
    (zip ([1 ..] :: [Integer]) fields)

createMatch ::
  TyCon ->
  Name ->
  Map
    TyCon
    [Field] ->
  (TyCon, [Field]) ->
  (TyCon, Expr Name ())
createMatch typeName funcVar items (tyCon, _) =
  let mismatches =
        (\(k, v) -> (k, noOpMatch k v))
          <$> M.toList (M.filterWithKey (\k _ -> k /= tyCon) items)
   in ( tyCon,
        MyLambda
          mempty
          "f"
          ( MyCaseMatch
              mempty
              (MyVar mempty (aName typeName))
              ( NE.fromList $
                  [ ( tyCon,
                      MyLambda mempty funcVar (MyConsApp mempty (MyConstructor mempty tyCon) (MyApp mempty (MyVar mempty "f") (MyVar mempty "a")))
                    )
                  ]
                    <> mismatches
              )
              Nothing
          )
      )
