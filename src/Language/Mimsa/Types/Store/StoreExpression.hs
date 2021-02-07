{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.Store.StoreExpression where

import qualified Data.Aeson as JSON
import Data.Swagger
import GHC.Generics
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.Bindings
import Language.Mimsa.Types.Store.TypeBindings

-- a storeExpression contains the AST Expr
-- and a map of names to hashes with further functions inside
data StoreExpression ann = StoreExpression
  { storeExpression :: Expr Name ann,
    storeBindings :: Bindings,
    storeTypeBindings :: TypeBindings
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON, Functor, ToSchema)
