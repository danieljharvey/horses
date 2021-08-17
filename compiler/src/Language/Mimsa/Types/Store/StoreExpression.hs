{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store.StoreExpression where

import qualified Data.Aeson as JSON
import Data.OpenApi
import GHC.Generics
import Language.Mimsa.Printer
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
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic,
      Functor
    )
  deriving anyclass
    ( JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

instance Printer (StoreExpression ann) where
  prettyPrint (StoreExpression expr _ _) = "{ expr: " <> prettyPrint expr <> " }"
