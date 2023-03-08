{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Types.Function (ArgumentName (..), 
      FunctionName (..), Function (..)) where

import Calc.Types.Expr
import Calc.Types.Type
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

newtype FunctionName = FunctionName Text
  deriving newtype (Eq, Ord, Show)

instance IsString FunctionName where
  fromString = FunctionName . T.pack

newtype ArgumentName = ArgumentName Text
  deriving newtype (Eq, Ord, Show)

instance IsString ArgumentName where
  fromString = ArgumentName . T.pack

data Function ann = Function
  { fnAnn :: ann, 
    fnArgs :: [(ArgumentName, Type ann)],
    fnFunctionName :: FunctionName,
    fnBody :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)
