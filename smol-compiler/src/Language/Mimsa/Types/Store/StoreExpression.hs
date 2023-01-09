{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Store.StoreExpression where

import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import GHC.Generics
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.ExprHash

-- a storeExpression contains the AST Expr
-- and a map of names to hashes with further functions inside
data StoreExpression ann
  = StoreExpression
      { seExpr :: Expr Name ann,
        seBindings :: Map (Maybe ModuleName, Name) ExprHash,
        seTypeBindings :: Map (Maybe ModuleName, TyCon) ExprHash,
        seInfixes :: Map InfixOp ExprHash,
        seTypes :: Map (Maybe ModuleName, TypeName) ExprHash
      }
  | StoreDataType
      { seDataType :: DataType,
        seTypes :: Map (Maybe ModuleName, TypeName) ExprHash
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
      JSON.FromJSON
    )

storeExpression :: StoreExpression ann -> Maybe (Expr Name ann)
storeExpression (StoreExpression se _ _ _ _) = Just se
storeExpression _ = Nothing

storeBindings :: StoreExpression ann -> Map (Maybe ModuleName, Name) ExprHash
storeBindings (StoreExpression _ bindings _ _ _) = bindings
storeBindings _ = mempty

storeTypeBindings :: StoreExpression ann -> Map (Maybe ModuleName, TyCon) ExprHash
storeTypeBindings (StoreExpression _ _ typeBindings _ _) = typeBindings
storeTypeBindings _ = mempty

storeInfixes :: StoreExpression ann -> Map InfixOp ExprHash
storeInfixes (StoreExpression _ _ _ infixes _) = infixes
storeInfixes _ = mempty

storeTypes :: StoreExpression ann -> Map (Maybe ModuleName, TypeName) ExprHash
storeTypes (StoreExpression _ _ _ _ types) = types
storeTypes (StoreDataType _ types) = types

instance Printer (StoreExpression ann) where
  prettyPrint (StoreExpression expr _ _ _ _) = prettyPrint expr
  prettyPrint (StoreDataType dt _) = prettyPrint dt
