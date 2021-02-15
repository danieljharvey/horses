{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.ApplicativeApply
  ( applicativeApply,
  )
where

import Control.Applicative
import Control.Monad.Except
import Data.Coerce
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Text (Text)
import Language.Mimsa.Printer
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
                (getFallback (fName tyCon) matches items)
            )
        )
    )

-- if we have more constructors than matches, make a fallback that returns
-- fName
getFallback :: Name -> NonEmpty a -> Map b c -> Maybe (Expr Name ())
getFallback varName matches constructors =
  if length matches < length constructors
    then Just (MyVar mempty varName)
    else Nothing

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
  case NE.nonEmpty $ NE.filter (\(_, as) -> containsVar funcVar as) constructors of
    Nothing -> throwError $ "No constructors contain " <> prettyPrint funcVar
    Just consts ->
      pure $
        createMatch
          typeName
          funcVar
          (getFallback (aName typeName) consts items)
          <$> consts

createMatch ::
  TyCon ->
  Name ->
  Maybe (Expr Name ()) ->
  (TyCon, [Field]) ->
  (TyCon, Expr Name ())
createMatch typeName funcVar fallback (tyCon, _) =
  ( tyCon,
    MyLambda
      mempty
      "f"
      ( MyCaseMatch
          mempty
          (MyVar mempty (aName typeName))
          ( NE.fromList
              [ ( tyCon,
                  MyLambda mempty funcVar (MyConsApp mempty (MyConstructor mempty tyCon) (MyApp mempty (MyVar mempty "f") (MyVar mempty "a")))
                )
              ]
          )
          fallback
      )
  )
