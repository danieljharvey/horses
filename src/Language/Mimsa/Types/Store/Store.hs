{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Store.Store where

import qualified Data.Aeson as JSON
import Data.Map (Map)
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
      JSON.FromJSON
    )
  deriving (Functor)
