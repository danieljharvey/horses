{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Typechecker.UniVar where

import GHC.Generics
import Language.Mimsa.Core
import Prettyprinter

newtype UniVar = UniVar Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num)

instance Printer UniVar where
  prettyDoc (UniVar a) = pretty a
