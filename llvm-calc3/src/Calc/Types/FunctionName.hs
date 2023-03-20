{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Types.FunctionName
  ( FunctionName (..),
  )
where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Prettyprinter as PP

newtype FunctionName = FunctionName Text
  deriving newtype (Eq, Ord, Show)

instance IsString FunctionName where
  fromString = FunctionName . T.pack

instance PP.Pretty FunctionName where
  pretty (FunctionName fn) = PP.pretty fn
