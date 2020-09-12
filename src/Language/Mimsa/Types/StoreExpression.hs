{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.StoreExpression where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Types.Bindings
import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.TypeBindings

-- a storeExpression contains the AST Expr
-- and a map of names to hashes with further functions inside
-- not sure whether to store the builtins we need here too?
data StoreExpression
  = StoreExpression
      { storeExpression :: Expr Name,
        storeBindings :: Bindings,
        storeTypeBindings :: TypeBindings
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON)
