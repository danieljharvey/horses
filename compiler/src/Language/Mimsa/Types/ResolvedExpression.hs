{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.ResolvedExpression where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.StoreExpression
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Unique

data ResolvedExpression ann = ResolvedExpression
  { reMonoType :: MonoType,
    reStoreExpression :: StoreExpression ann,
    reVarExpression :: Expr (Name, Unique) ann,
    reSwaps :: Swaps,
    reTypedExpression :: Expr (Name, Unique) MonoType,
    reInput :: Text
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.ToJSON)
