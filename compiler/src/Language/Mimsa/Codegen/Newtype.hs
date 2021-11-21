{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Codegen.Newtype
  ( wrap,
    unwrap,
  )
where

--import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- | A newtype is a datatype with one constructor
-- | with one argument
wrap :: DataType -> Either Text (Expr Name ())
wrap (DataType _tyCon _vars items) =
  case getOnlyMapItem items of
    Nothing -> Left "Type should have one constructor"
    Just (consName, [_a]) ->
      Right
        ( MyLambda
            mempty
            (Identifier mempty "a")
            ( MyApp
                mempty
                (MyConstructor mempty consName)
                (MyVar mempty "a")
            )
        )
    Just (_, _) -> Left "Constructor should only have one argument"

-- | A newtype is a datatype with one constructor
-- | with one argument
unwrap :: DataType -> Either Text (Expr Name ())
unwrap (DataType tyCon _vars items) = do
  let tyName = tyConToName tyCon
  case getOnlyMapItem items of
    Nothing -> Left "Type should have one constructor"
    Just (consName, [_a]) ->
      Right
        ( MyLambda
            mempty
            (Identifier mempty tyName)
            ( MyPatternMatch
                mempty
                (MyVar mempty tyName)
                [ ( PConstructor mempty consName [PVar mempty "a"],
                    MyVar mempty "a"
                  )
                ]
            )
        )
    Just (_, _) -> Left "Constructor should only have one argument"

getOnlyMapItem :: Map k a -> Maybe (k, a)
getOnlyMapItem items = case M.toList items of
  [(k, a)] -> pure (k, a)
  _ -> Nothing
