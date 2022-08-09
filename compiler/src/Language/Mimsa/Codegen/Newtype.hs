{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Codegen.Newtype
  ( wrap,
    unwrap,
  )
where

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.CodegenError
import Language.Mimsa.Types.Identifiers

-- | A newtype is a datatype with one constructor
-- | with one argument
wrap :: DataType -> Either CodegenError (Expr Name ())
wrap (DataType _tyCon _vars items) = do
  item <- getOnlyMapItem items
  case item of
    (consName, [_a]) ->
      pure
        ( MyLambda
            mempty
            (Identifier mempty "a")
            ( MyApp
                mempty
                (MyConstructor mempty Nothing consName)
                (MyVar mempty Nothing "a")
            )
        )
    (_, _) -> throwError NewtypeShouldOnlyHaveOneArgument

-- | A newtype is a datatype with one constructor
-- | with one argument
unwrap :: DataType -> Either CodegenError (Expr Name ())
unwrap (DataType tyCon _vars items) = do
  let tyName = typeNameToName tyCon
  item <- getOnlyMapItem items
  case item of
    (consName, [_a]) ->
      pure
        ( MyLambda
            mempty
            (Identifier mempty tyName)
            ( MyPatternMatch
                mempty
                (MyVar mempty Nothing tyName)
                [ ( PConstructor mempty Nothing consName [PVar mempty "a"],
                    MyVar mempty Nothing "a"
                  )
                ]
            )
        )
    (_, _) -> throwError NewtypeShouldOnlyHaveOneArgument

getOnlyMapItem :: Map k a -> Either CodegenError (k, a)
getOnlyMapItem items = case M.toList items of
  [(k, a)] -> pure (k, a)
  [] -> throwError NoConstructorMatches
  _ -> throwError TooManyConstructorMatches
