{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Typechecker.UniVar where

import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer

newtype UniVar = UniVar Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num)

instance Printer UniVar where
  prettyDoc (UniVar a) = pretty a
