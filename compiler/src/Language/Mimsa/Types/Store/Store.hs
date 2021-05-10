{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store.Store where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Swagger
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Store.StoreExpression

-- store is where we keep the big map of hashes to expresions
newtype Store ann = Store {getStore :: Map ExprHash (StoreExpression ann)}
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup,
      Monoid,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )
  deriving (Functor)

instance Printer (Store ann) where
  prettyPrint (Store store) = T.intercalate ", " (prettyPrint <$> M.keys store)
