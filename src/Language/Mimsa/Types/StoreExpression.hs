{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.StoreExpression where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Bindings
import Language.Mimsa.Types.Name

-- a storeExpression contains the AST Expr
-- and a map of names to hashes with further functions inside
-- not sure whether to store the builtins we need here too?
data StoreExpression
  = StoreExpression
      { storeBindings :: Bindings,
        storeExpression :: Expr Name
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON)
