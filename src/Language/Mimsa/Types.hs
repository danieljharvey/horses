{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types
  ( ExprHash (..),
    StoreEnv (..),
    module Language.Mimsa.Types.Name,
    module Language.Mimsa.Types.AST,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Name

------------

newtype ExprHash = ExprHash Int
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.FromJSON, JSON.ToJSON)

-------

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data StoreEnv
  = StoreEnv
      { items :: M.Map ExprHash Expr,
        bindings :: M.Map Name ExprHash
      }

instance Semigroup StoreEnv where
  StoreEnv a a' <> StoreEnv b b' = StoreEnv (a <> b) (a' <> b')

instance Monoid StoreEnv where
  mempty = StoreEnv mempty mempty
--------

-- a storeExpression contains the AST Expr
-- and a map of names to hashes with further functions inside
-- not sure whether to store the builtins we need here too?
-- data StoreExpression = StoreExpression
