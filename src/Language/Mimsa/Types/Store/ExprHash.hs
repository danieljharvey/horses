{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Store.ExprHash where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Text (Text)
import Language.Mimsa.Printer
import Servant.API

newtype ExprHash = ExprHash Text
  deriving (Eq, Ord)
  deriving newtype (JSON.FromJSON, JSON.ToJSON, FromHttpApiData)

instance Show ExprHash where
  show (ExprHash a) = T.unpack a

instance Printer ExprHash where
  prettyPrint (ExprHash a) = a
