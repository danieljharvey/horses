{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.ForeignFunc
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer

------------

newtype ExprHash = ExprHash Int
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.FromJSON, JSON.ToJSON)

instance Printer ExprHash where
  prettyPrint (ExprHash a) = T.pack . show $ a

-------

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data StoreEnv
  = StoreEnv
      { store :: Store,
        bindings :: Bindings
      }
  deriving (Eq, Ord, Show)

instance Semigroup StoreEnv where
  StoreEnv a a' <> StoreEnv b b' = StoreEnv (a <> b) (a' <> b')

instance Monoid StoreEnv where
  mempty = StoreEnv mempty mempty

--------

-- store is where we keep the big map of hashes to expresions
newtype Store = Store {getStore :: Map ExprHash StoreExpression}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

-- our built-in functions for doing IO things etc
-- statically defined and made available in all computations for now
newtype Library = Library {getLibrary :: Map FuncName ForeignFunc}

-- a list of names to hashes
newtype Bindings = Bindings {getBindings :: Map Name ExprHash}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, JSON.FromJSON, JSON.ToJSON)

instance Printer Bindings where
  prettyPrint (Bindings b) = "{ " <> T.intercalate ", " (prettyPrint <$> M.keys b) <> " }"

-- the names that get changed in substitution
type Swaps = Map Name Name

-- a storeExpression contains the AST Expr
-- and a map of names to hashes with further functions inside
-- not sure whether to store the builtins we need here too?
data StoreExpression
  = StoreExpression
      { storeBindings :: Bindings,
        storeExpression :: Expr Name
      }
  deriving (Eq, Ord, Show, Generic)
  deriving (JSON.ToJSON, JSON.FromJSON)
