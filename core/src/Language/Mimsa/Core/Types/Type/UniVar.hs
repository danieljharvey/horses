{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Core.Types.Type.UniVar where

import GHC.Generics
import Language.Mimsa.Core.Printer
import Prettyprinter

newtype UniVar = UniVar Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num)

instance Printer UniVar where
  prettyDoc (UniVar a) = pretty a
