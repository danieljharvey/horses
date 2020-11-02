{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Store.ExprHash where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Language.Mimsa.Printer

newtype ExprHash = ExprHash Int
  deriving (Eq, Ord)
  deriving newtype (JSON.FromJSON, JSON.ToJSON)

instance Show ExprHash where
  show (ExprHash a) = show a

instance Printer ExprHash where
  prettyPrint (ExprHash a) = T.pack . show $ a
