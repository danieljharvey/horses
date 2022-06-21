{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Store.StoreExpression where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Store.TypeBindings

-- a storeExpression contains the AST Expr
-- and a map of names to hashes with further functions inside
data StoreExpression ann = StoreExpression
  { storeExpression :: Expr Name ann,
    storeBindings :: Map (Maybe ModuleName, Name) ExprHash,
    storeTypeBindings :: TypeBindings,
    storeInfixes :: Map InfixOp ExprHash
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

instance Printer (StoreExpression ann) where
  prettyPrint (StoreExpression expr _ _ _) = prettyPrint expr
