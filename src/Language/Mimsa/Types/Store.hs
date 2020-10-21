{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Store where

import Data.Map (Map)
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.StoreExpression

-- store is where we keep the big map of hashes to expresions
newtype Store ann = Store {getStore :: Map ExprHash (StoreExpression ann)}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)
  deriving (Functor)
