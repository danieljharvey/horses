{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.UniVar where

import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer

newtype UniVar = UniVar Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num)

instance Printer UniVar where
  prettyPrint (UniVar a) = T.pack . show $ a
